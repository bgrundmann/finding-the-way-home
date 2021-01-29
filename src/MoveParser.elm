module MoveParser exposing (Definitions, parseMoves, validatePileName)

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
        , options : List Argument
        }
    | DuplicateDefinition String
    | Problem String


type Expectation
    = EPileName
    | ENumberName
    | EEndOfInput
    | EKeyword String
    | EEndOfLine
    | EMoveName


type alias Definitions =
    Dict String MoveDefinition


{-| The ParseEnv guides the parsers into making decisions. Unlike context which is
used by to augment the error messages. That said if the Elm parser library provided
a way to read the context I would have used that.
-}
type alias ParseEnv =
    { whereAreWe : WhereAreWe -- Tells us if End Of Input is ok or not
    , definitions : Definitions
    }


keywords =
    Set.fromList [ "repeat", "end", "def", "ignore", "doc" ]


keyword string =
    Parser.Advanced.keyword (Token string (Expected (EKeyword string)))


keywordEnd =
    keyword "end"


keywordDef =
    keyword "def"


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


type WhereAreWe
    = Toplevel
    | Embedded


{-| The end of a statement is a newline character. Or at the toplevel the end of the file is also ok.
-}
endOfStatement : ParseEnv -> Parser ()
endOfStatement env =
    case env.whereAreWe of
        Toplevel ->
            oneOf [ newline, end (Expected EEndOfInput) ]

        Embedded ->
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


exprParser : List Argument -> Maybe MoveDefinition -> Argument -> Parser Expr
exprParser argumentsOfEnclosingDefinition move expectedArgument =
    {- TODO: add expected argument here -}
    let
        maybeArgument whenNot name =
            case List.Extra.find (\( _, a ) -> a.name == name) (List.indexedMap Tuple.pair argumentsOfEnclosingDefinition) of
                Nothing ->
                    whenNot name

                Just ( ndx, arg ) ->
                    succeed (ExprArgument { name = name, ndx = ndx, kind = arg.kind })

        enclosingArgumentsOfTheRightKind =
            List.filter (\a -> a.kind == expectedArgument.kind) argumentsOfEnclosingDefinition

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


doMoveParser : ParseEnv -> List Argument -> Parser (Move Expr)
doMoveParser env argumentsOfEnclosingDefinition =
    moveNameParser
        |> andThen (lookupDefinition env.definitions)
        |> andThen (actualsParser argumentsOfEnclosingDefinition)


actualsParser : List Argument -> MoveDefinition -> Parser (Move Expr)
actualsParser argumentsOfEnclosingDefinition moveDefinition =
    let
        helper actuals expectedArgs =
            case expectedArgs of
                [] ->
                    succeed (Do moveDefinition (List.reverse actuals))

                expectedArg :: restExpectedArgs ->
                    (exprParser
                        argumentsOfEnclosingDefinition
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


repeatParser : ParseEnv -> List Argument -> Parser (Move Expr)
repeatParser env arguments =
    succeed (\n moves -> Repeat n moves)
        |. keywordRepeat
        |. spaces
        |= exprParser arguments Nothing { name = "N", kind = KindInt }
        |. spaces
        |. newline
        |= movesParser { env | whereAreWe = Embedded } arguments
        |. keywordEnd


moveParser : ParseEnv -> List Argument -> Parser (Maybe (Move Expr))
moveParser env arguments =
    succeed (\ignoref p -> ignoref p)
        |= oneOf
            [ (keyword "ignore" |. spaces) |> map (\() -> always Nothing)
            , succeed () |> map (\() -> Just)
            ]
        |= oneOf
            [ repeatParser env arguments
            , doMoveParser env arguments
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



{-
   let
       actualLen =
           List.length actualArgs

       expectedLen =
           List.length args
   in
   if expectedLen < actualLen then
       problem (Problem (Move.signature d ++ ", more arguments then expected"))

   else if actualLen < expectedLen then
       problem (Problem (Move.signature d ++ ", less arguments then expected"))

   else
       succeed (Do d actualArgs)
-}


movesParser : ParseEnv -> List Argument -> Parser (List (Move Expr))
movesParser env arguments =
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
                    |= moveParser env arguments
                    |. endOfStatement env
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


definitionsParser : ParseEnv -> Parser Definitions
definitionsParser env =
    let
        helper ( localDefs, e ) =
            oneOf
                [ succeed
                    (\def ->
                        let
                            newLocalDefs =
                                Dict.insert def.name def localDefs

                            newDefinitions =
                                Dict.insert def.name def e.definitions
                        in
                        Loop ( newLocalDefs, { e | definitions = newDefinitions } )
                    )
                    |= definitionParser e
                , succeed () |> map (\() -> Done localDefs)
                ]
    in
    loop ( Dict.empty, env ) helper


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
                    (\doc moves ->
                        { name = name
                        , args = args
                        , body = UserDefined { moves = moves, definitions = [] }
                        , doc = doc
                        }
                    )
                    |= docParser
                    |= movesParser { env | whereAreWe = Embedded } args
                    |. keywordEnd
                    |. endOfStatement env
            )


definitionsAndMoves : ParseEnv -> Parser ( Definitions, List (Move Expr) )
definitionsAndMoves env =
    definitionsParser env
        |> andThen
            (\defs ->
                movesParser { env | definitions = Dict.union defs env.definitions } []
                    |> map (\moves -> ( defs, moves ))
            )


parser : ParseEnv -> Parser { definitions : Definitions, moves : List (Move ExprValue) }
parser env =
    definitionsAndMoves env
        |> andThen
            (\( defs, moves ) ->
                case Move.substituteArguments identity [] moves of
                    Err msg ->
                        problem (Problem msg)

                    Ok moves2 ->
                        succeed { definitions = defs, moves = moves2 }
            )


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

                Problem msg ->
                    msg

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

                                        l ->
                                            " or one of " ++ String.join ", " (List.map .name l)
                                   )

                        KindPile ->
                            moveSignature
                                ++ "\n\n"
                                ++ doc
                                ++ "\n\nI need a pilename for "
                                ++ argName
                                ++ "\n"
                                ++ "These are the piles I know about: "
                                ++ String.join ", " (List.map .name options)

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


parseMoves : Definitions -> String -> Result String { moves : List (Move ExprValue), definitions : Definitions }
parseMoves primitives text =
    case run (parser { definitions = primitives, whereAreWe = Toplevel } |. end (Expected EEndOfInput)) text of
        Ok m ->
            Ok m

        Err deadEnds ->
            Err (deadEndsToString text deadEnds)


{-| Nothing if s is a valid pilename, Just errorMessage otherwise
-}
validatePileName : String -> Maybe String
validatePileName s =
    case run (pileNameParser (Expected EPileName) |. end (Expected EEndOfInput)) s of
        Ok res ->
            Nothing

        Err _ ->
            Just "A pilename should look like this: deck, deck2, table, ..."
