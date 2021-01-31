module MoveParser exposing (Definitions, definitionsFromList, parseMoves, validatePileName)

import Char
import Dict exposing (Dict)
import Dict.Extra
import List
import List.Extra
import Move
    exposing
        ( Argument
        , ArgumentKind(..)
        , Expr(..)
        , ExprValue(..)
        , Location
        , Move(..)
        , MoveDefinition
        , UserDefinedOrPrimitive(..)
        )
import Parser.Advanced
    exposing
        ( (|.)
        , (|=)
        , Step(..)
        , Token(..)
        , andThen
        , chompWhile
        , end
        , getChompedString
        , int
        , loop
        , map
        , oneOf
        , problem
        , run
        , succeed
        , token
        , variable
        )
import Set


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type alias Context =
    ()


type alias DeadEnd =
    Parser.Advanced.DeadEnd Context Problem


type Problem
    = UnknownMove String
    | NoSuchArgument String
    | Expected Expectation
    | ExpectedForArgument
        { move : Maybe MoveDefinition -- Nothing => Repeat
        , argName : String
        , argKind : ArgumentKind
        , options : List String
        }
    | DuplicateDefinition String


type Expectation
    = EPileName
    | ENumberName
    | EEndOfInput
    | EKeyword String
    | EEndOfLine
    | EMoveName


type alias Definitions =
    Dict String MoveDefinition


definitionsFromList : List MoveDefinition -> Definitions
definitionsFromList l =
    l
        |> List.map (\d -> ( d.name, d ))
        |> Dict.fromList


{-| The ParseEnv guides the parsers into making decisions. Unlike context which is
used by to augment the error messages. That said if the Elm parser library provided
a way to read the context during parsing we could have used that.
-}
type alias ParseEnv =
    { level : Int -- 0 == toplevel, 1 == first def, ... (note that Argument.up is the other way around)
    , toplevel : Bool
    , definitions : Definitions -- A dictionary of all the moves that are in scope, local and global
    , arguments :
        Dict String
            { level : Int
            , ndx : Int
            , kind : ArgumentKind
            }

    -- arguments to the current scope
    }


addDefinition : ParseEnv -> MoveDefinition -> ParseEnv
addDefinition env md =
    { env | definitions = Dict.insert md.name md env.definitions }


lookupArgument : ParseEnv -> String -> Maybe { name : String, ndx : Int, up : Int, kind : ArgumentKind }
lookupArgument env name =
    Dict.get name env.arguments
        |> Maybe.map
            (\{ level, ndx, kind } ->
                { name = name, ndx = ndx, up = env.level - level, kind = kind }
            )


argumentsInScopeOfKind : ParseEnv -> ArgumentKind -> List String
argumentsInScopeOfKind env kind =
    Dict.toList env.arguments
        |> List.filter (\( _, a ) -> a.kind == kind)
        |> List.map Tuple.first


enterDefinition : ParseEnv -> List { name : String, kind : ArgumentKind } -> ParseEnv
enterDefinition env arguments =
    let
        thisLevel =
            env.level + 1

        localArguments =
            List.indexedMap
                (\ndx { name, kind } ->
                    ( name, { kind = kind, ndx = ndx, level = thisLevel } )
                )
                arguments
                |> Dict.fromList

        newArguments =
            Dict.union localArguments env.arguments
    in
    { env | arguments = newArguments, level = thisLevel, toplevel = False }


enterRepeat : ParseEnv -> ParseEnv
enterRepeat env =
    { env | toplevel = False }


toplevelEnv : Definitions -> ParseEnv
toplevelEnv primitives =
    { level = 0
    , toplevel = True
    , definitions = primitives
    , arguments = Dict.empty
    }


keywords : Set.Set String
keywords =
    Set.fromList [ "repeat", "end", "def", "ignore", "doc" ]


keyword : String -> Parser ()
keyword string =
    Parser.Advanced.keyword (Token string (Expected (EKeyword string)))


keywordEnd : Parser ()
keywordEnd =
    keyword "end"


keywordDef : Parser ()
keywordDef =
    keyword "def"


keywordRepeat : Parser ()
keywordRepeat =
    keyword "repeat"


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ' || c == '\t')


{-| At least one newline, maybe preceeded by whitespace, and possibly followed by more newlines / spaces.
-}
newline : Parser ()
newline =
    spaces
        |. token (Token "\n" (Expected EEndOfLine))
        |. spaces
        |. loop ()
            (\() ->
                oneOf
                    [ (token (Token "\n" (Expected EEndOfLine)) |. spaces) |> map (\_ -> Loop ())
                    , succeed () |> map (\_ -> Done ())
                    ]
            )


{-| The end of a statement is a newline character. Or at the toplevel the end of the file is also ok.
-}
endOfStatement : ParseEnv -> Parser ()
endOfStatement env =
    if env.toplevel then
        oneOf [ newline, end (Expected EEndOfInput) ]

    else
        newline


pileNameParser : Problem -> Parser String
pileNameParser problem =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '_', reserved = Set.empty, expecting = problem }


numberNameParser : Problem -> Parser String
numberNameParser problem =
    variable { start = Char.isUpper, inner = \c -> Char.isUpper c || c == '_', reserved = Set.empty, expecting = problem }


moveNameParser : Parser String
moveNameParser =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '-', reserved = keywords, expecting = Expected EMoveName }


exprParser : ParseEnv -> Maybe MoveDefinition -> Argument -> Parser Expr
exprParser env move expectedArgument =
    let
        maybeArgument whenNot name =
            case lookupArgument env name of
                Nothing ->
                    whenNot name

                Just arg ->
                    succeed (ExprArgument arg)

        enclosingArgumentsOfTheRightKind =
            argumentsInScopeOfKind env expectedArgument.kind

        argProblem =
            ExpectedForArgument
                { move = move
                , argName = expectedArgument.name
                , argKind = expectedArgument.kind
                , options = enclosingArgumentsOfTheRightKind
                }
    in
    case expectedArgument.kind of
        KindInt ->
            oneOf
                [ int argProblem argProblem |> map (\i -> ExprValue (Int i))
                , numberNameParser argProblem |> andThen (maybeArgument (\n -> problem (NoSuchArgument n)))
                ]

        KindPile ->
            pileNameParser argProblem |> andThen (maybeArgument (\n -> succeed (ExprValue (Pile n))))


getLocation : Parser Location
getLocation =
    Parser.Advanced.getRow |> map (\row -> { row = row })


doMoveParser : ParseEnv -> Parser Move
doMoveParser env =
    getLocation
        |> andThen
            (\location ->
                moveNameParser
                    |> andThen (lookupDefinition env.definitions)
                    |> andThen (actualsParser env location)
            )


actualsParser : ParseEnv -> Location -> MoveDefinition -> Parser Move
actualsParser env location moveDefinition =
    let
        helper actuals expectedArgs =
            case expectedArgs of
                [] ->
                    succeed (Do location moveDefinition (List.reverse actuals))

                expectedArg :: restExpectedArgs ->
                    (exprParser
                        env
                        (Just moveDefinition)
                        expectedArg
                        |. spaces
                    )
                        |> andThen
                            (\expr ->
                                helper (expr :: actuals) restExpectedArgs
                            )
    in
    succeed identity
        |. spaces
        |= helper [] moveDefinition.args


repeatParser : ParseEnv -> Parser Move
repeatParser env =
    succeed (\location n moves -> Repeat location n moves)
        |= getLocation
        |. keywordRepeat
        |. spaces
        |= exprParser env Nothing { name = "N", kind = KindInt }
        |. spaces
        |. newline
        |= movesParser (enterRepeat env)
        |. keywordEnd


moveParser : ParseEnv -> Parser (Maybe Move)
moveParser env =
    succeed (\ignoref p -> ignoref p)
        |= oneOf
            [ (keyword "ignore" |. spaces) |> map (\() -> always Nothing)
            , succeed () |> map (\() -> Just)
            ]
        |= oneOf
            [ repeatParser env
            , doMoveParser env
            ]


argsParser : Parser (List Argument)
argsParser =
    let
        helper result =
            oneOf
                [ succeed (\arg -> Loop (arg :: result))
                    |= argParser
                    |. spaces
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


{-| A argument as it occurrs in the def line of a move definition.
-}
argParser : Parser Argument
argParser =
    oneOf
        [ pileNameParser (Expected EPileName) |> map (\n -> { name = n, kind = KindPile })
        , numberNameParser (Expected ENumberName) |> map (\n -> { name = n, kind = KindInt })
        ]


{-| We just parsed a move name and are looking up the corresponding move.
-}
lookupDefinition : Definitions -> String -> Parser MoveDefinition
lookupDefinition definitions moveName =
    case Dict.get moveName definitions of
        Nothing ->
            problem (UnknownMove moveName)

        Just d ->
            succeed d


movesParser : ParseEnv -> Parser (List Move)
movesParser env =
    let
        helper result =
            oneOf
                [ succeed
                    (\maybeMove ->
                        case maybeMove of
                            Nothing ->
                                Loop result

                            Just move ->
                                Loop (move :: result)
                    )
                    |= moveParser env
                    |. endOfStatement env
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


definitionsParser : ParseEnv -> Parser (List MoveDefinition)
definitionsParser env =
    let
        helper ( localDefs, e ) =
            oneOf
                [ succeed
                    (\def ->
                        Loop ( def :: localDefs, addDefinition e def )
                    )
                    |= definitionParser e
                , succeed () |> map (\() -> Done localDefs)
                ]
    in
    loop ( [], env ) helper


defLineParser : ParseEnv -> Parser { name : String, args : List Argument }
defLineParser env =
    succeed (\name args -> { name = name, args = args })
        |. keywordDef
        |. spaces
        |= (moveNameParser
                |> andThen
                    (\n ->
                        if Dict.member n env.definitions then
                            problem (DuplicateDefinition n)

                        else
                            succeed n
                    )
           )
        |. spaces
        |= argsParser
        |. newline


docParser : Parser String
docParser =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed identity
                |. keyword "doc"
                |. spaces
                |= (chompWhile (\c -> c /= '\n') |> getChompedString)
                |. newline
            , succeed () |> map (always "")
            ]


definitionParser : ParseEnv -> Parser MoveDefinition
definitionParser env =
    defLineParser env
        |> andThen
            (\{ name, args } ->
                succeed
                    (\doc ( definitions, moves ) ->
                        { name = name
                        , args = args
                        , body =
                            UserDefined
                                { moves = moves
                                , definitions = definitions
                                }
                        , doc = doc
                        }
                    )
                    |= docParser
                    |= definitionsAndMoves (enterDefinition env args)
                    |. keywordEnd
                    |. endOfStatement env
            )


definitionsAndMoves : ParseEnv -> Parser ( List MoveDefinition, List Move )
definitionsAndMoves env =
    definitionsParser env
        |> andThen
            (\defs ->
                movesParser { env | definitions = Dict.union (definitionsFromList defs) env.definitions }
                    |> map (\moves -> ( defs, moves ))
            )


parser : ParseEnv -> Parser { definitions : List MoveDefinition, moves : List Move }
parser env =
    succeed (\( defs, moves ) -> { definitions = defs, moves = moves })
        |= definitionsAndMoves env


gatherDeadEndsByLocation : List DeadEnd -> List { row : Int, col : Int, problems : List Problem }
gatherDeadEndsByLocation deadEnds =
    Dict.Extra.groupBy (\d -> ( d.row, d.col )) deadEnds
        |> Dict.toList
        |> List.map
            (\( ( row, col ), ds ) ->
                { row = row, col = col, problems = List.map .problem ds }
            )


deadEndsToString : String -> List DeadEnd -> String
deadEndsToString text deadEnds =
    let
        expectationToString ex =
            case ex of
                EKeyword s ->
                    "'" ++ s ++ "'"

                EPileName ->
                    "a pile name (e.g. deck)"

                ENumberName ->
                    "a number name (e.g. N)"

                EEndOfLine ->
                    "the next line"

                EEndOfInput ->
                    "the end"

                EMoveName ->
                    "a move name (e.g. 'deal')"

        problemToString problem =
            case problem of
                UnknownMove n ->
                    "Don't know how to do '" ++ n ++ "'"

                DuplicateDefinition d ->
                    "You already know how to '" ++ d ++ "'"

                Expected ex ->
                    "Expected " ++ expectationToString ex

                NoSuchArgument name ->
                    name ++ " looks like an argument, but no such argument was defined"

                ExpectedForArgument { move, argName, argKind, options } ->
                    let
                        ( moveSignature, doc ) =
                            case move of
                                Nothing ->
                                    ( Move.repeatSignature, "Repeat the given moves N times" )

                                Just m ->
                                    ( Move.signature m, m.doc )
                    in
                    case argKind of
                        KindInt ->
                            moveSignature
                                ++ "\n\n"
                                ++ doc
                                ++ "\n\nI need a number for "
                                ++ argName
                                ++ "\n"
                                ++ "Type the number (e.g. 52)"
                                ++ (case options of
                                        [] ->
                                            ""

                                        [ x ] ->
                                            " or " ++ x

                                        l ->
                                            " or one of " ++ String.join ", " l
                                   )

                        KindPile ->
                            moveSignature
                                ++ "\n\n"
                                ++ doc
                                ++ "\n\nI need a pilename for "
                                ++ argName
                                ++ "\n"
                                ++ "These are the piles I know about: "
                                ++ String.join ", " options

        relevantLineAndPlace row col =
            case List.Extra.getAt (row - 1) (String.lines text) of
                Nothing ->
                    "THIS SHOULD NOT HAPPEN"

                Just line ->
                    line ++ "\n" ++ String.repeat (col - 1) " " ++ "^\n"

        deadEndToString { row, col, problems } =
            let
                ( wrappedExpectedProblems, otherProblems ) =
                    List.partition
                        (\p ->
                            case p of
                                Expected _ ->
                                    True

                                _ ->
                                    False
                        )
                        problems

                expectedProblems =
                    List.map
                        (\e ->
                            case e of
                                Expected x ->
                                    x

                                _ ->
                                    -- Can't happen
                                    EKeyword ""
                        )
                        wrappedExpectedProblems

                expectedProblemsString =
                    case List.reverse expectedProblems of
                        [] ->
                            "\n"

                        [ ex ] ->
                            "Expected " ++ expectationToString ex ++ "\n"

                        ex :: exs ->
                            "Expected one of " ++ String.join ", " (List.map expectationToString (List.reverse exs)) ++ " or " ++ expectationToString ex ++ "\n"

                otherProblemsString =
                    case otherProblems of
                        [] ->
                            "\n"

                        others ->
                            String.join "\n" (List.map problemToString others)
            in
            relevantLineAndPlace row col ++ expectedProblemsString ++ otherProblemsString
    in
    case gatherDeadEndsByLocation deadEnds of
        [] ->
            ""

        des ->
            String.join "\n" (List.map deadEndToString des)


{-| Parse a list of definitions and moves as written at the toplevel of a program.
-}
parseMoves :
    Definitions
    -> String
    -> Result String { moves : List Move, definitions : List MoveDefinition }
parseMoves primitives text =
    let
        env =
            toplevelEnv primitives
    in
    case run (parser env |. end (Expected EEndOfInput)) text of
        Ok m ->
            Ok m

        Err deadEnds ->
            Err (deadEndsToString text deadEnds)


{-| Nothing if s is a valid pilename, Just errorMessage otherwise
-}
validatePileName : String -> Maybe String
validatePileName s =
    case run (pileNameParser (Expected EPileName) |. end (Expected EEndOfInput)) s of
        Ok _ ->
            Nothing

        Err _ ->
            Just "A pilename should look like this: deck, deck2, table, ..."
