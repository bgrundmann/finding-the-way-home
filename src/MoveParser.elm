module MoveParser exposing (Definitions, parseMoves)

import Char
import Dict exposing (Dict)
import Dict.Extra
import List
import List.Extra
import Move exposing (Argument, ArgumentKind(..), Expr(..), ExprValue(..), Move(..), MoveDefinition, MovesOrPrimitive(..))
import Parser.Advanced exposing ((|.), (|=), Step(..), Token(..), andThen, chompWhile, end, int, loop, map, oneOf, problem, run, succeed, token, variable)
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
        { moveName : String
        , moveSignature : String
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


keywords =
    Set.fromList [ "repeat", "end", "def" ]


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
endOfStatement : WhereAreWe -> Parser ()
endOfStatement whereAreWe =
    case whereAreWe of
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


exprParser : List Argument -> String -> String -> Argument -> Parser Expr
exprParser argumentsOfEnclosingDefinition moveName moveSignature expectedArgument =
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
                { moveName = moveName
                , moveSignature = moveSignature
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


doMoveParser : Definitions -> List Argument -> Parser (Move Expr)
doMoveParser definitions argumentsOfEnclosingDefinition =
    moveNameParser
        |> andThen (lookupDefinition definitions)
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
                        moveDefinition.name
                        (Move.signature moveDefinition)
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


repeatParser : Definitions -> List Argument -> Parser (Move Expr)
repeatParser definitions arguments =
    succeed (\n moves -> Repeat n moves)
        |. keywordRepeat
        |. spaces
        |= exprParser arguments "repeat" Move.repeatSignature { name = "N", kind = KindInt }
        |. spaces
        |. newline
        |= movesParser definitions arguments Embedded
        |. keywordEnd


moveParser : Definitions -> List Argument -> Parser (Move Expr)
moveParser definitions arguments =
    succeed identity
        |. spaces
        |= oneOf
            [ repeatParser definitions arguments
            , doMoveParser definitions arguments
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


movesParser : Definitions -> List Argument -> WhereAreWe -> Parser (List (Move Expr))
movesParser definitions arguments whereAreWe =
    let
        helper result =
            oneOf
                [ succeed (\cmd -> Loop (cmd :: result))
                    |= moveParser definitions arguments
                    |. endOfStatement whereAreWe
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


definitionsParser : Dict String MoveDefinition -> Parser Definitions
definitionsParser primitives =
    let
        helper definitions =
            oneOf
                [ succeed (\def -> Loop (Dict.insert def.name def definitions))
                    |= definitionParser definitions
                , succeed () |> map (\() -> Done definitions)
                ]
    in
    loop primitives helper


defLineParser : Definitions -> Parser { name : String, args : List Argument }
defLineParser definitions =
    succeed (\name args -> { name = name, args = args })
        |. keywordDef
        |. spaces
        |= (moveNameParser
                |> andThen
                    (\n ->
                        if Dict.member n definitions then
                            problem (DuplicateDefinition n)

                        else
                            succeed n
                    )
           )
        |. spaces
        |= argsParser
        |. newline


definitionParser : Definitions -> Parser MoveDefinition
definitionParser definitions =
    defLineParser definitions
        |> andThen
            (\{ name, args } ->
                succeed (\moves -> { name = name, args = args, movesOrPrimitive = Moves moves })
                    |= movesParser definitions args Embedded
                    |. keywordEnd
                    |. endOfStatement Toplevel
            )


definitionsAndMoves : Dict String MoveDefinition -> Parser ( Definitions, List (Move Expr) )
definitionsAndMoves primitives =
    definitionsParser primitives
        |> andThen
            (\defs ->
                movesParser defs [] Toplevel
                    |> map (\moves -> ( defs, moves ))
            )


parser : Dict String MoveDefinition -> Parser { definitions : Definitions, moves : List (Move ExprValue) }
parser primitives =
    definitionsAndMoves primitives
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

                ExpectedForArgument { moveName, moveSignature, argName, argKind, options } ->
                    case argKind of
                        KindInt ->
                            moveSignature
                                ++ "\n\nTo "
                                ++ moveName
                                ++ " I need a number for "
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
                                ++ "\n\nTo "
                                ++ moveName
                                ++ " I need a pilename for "
                                ++ argName
                                ++ "\n"
                                ++ "These are the piles I know about: "
                                ++ String.join ", " (List.map .name options)

        relevantLineAndPlace row col =
            case List.Extra.getAt (row - 1) (String.lines text) of
                Nothing ->
                    "THIS SHOULD NOT HAPPEN"

                Just line ->
                    String.fromInt row ++ "x" ++ String.fromInt col ++ "\n" ++ line ++ "\n" ++ String.repeat (col - 1) " " ++ "^\n"

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
    case run (parser primitives |. end (Expected EEndOfInput)) text of
        Ok m ->
            Ok m

        Err deadEnds ->
            Err (deadEndsToString text deadEnds)
