module MoveParser exposing (parseMoves)

import Array exposing (Array)
import Char
import Dict exposing (Dict)
import Dict.Extra
import Image exposing (PileName)
import List
import List.Extra
import Move exposing (..)
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
    | Expected Expectation
    | DuplicateDefinition String
    | Problem String


type Expectation
    = EInt
    | EPileName
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


pileNameParser : Parser String
pileNameParser =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '_', reserved = Set.empty, expecting = Expected EPileName }


numberNameParser : Parser String
numberNameParser =
    variable { start = Char.isUpper, inner = \c -> Char.isUpper c || c == '_', reserved = Set.empty, expecting = Expected ENumberName }


moveNameParser : Parser String
moveNameParser =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '-', reserved = keywords, expecting = Expected EMoveName }


exprParser : Parser Expr
exprParser =
    oneOf
        [ int (Expected EInt) (Expected EInt) |> map Int
        , pileNameParser |> map Pile
        ]
        |> map ExprValue


doMoveParser : Parser ( String, List Expr )
doMoveParser =
    succeed (\cmd args -> ( cmd, args ))
        |= moveNameParser
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= actualsParser
        |. oneOf [ token (Token "\n" (Expected EEndOfLine)), end (Expected EEndOfInput) ]


repeatParser : Definitions -> Parser (Move Expr)
repeatParser definitions =
    succeed (\n moves -> Repeat n moves)
        |. keywordRepeat
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= exprParser
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. token (Token "\n" (Expected EEndOfLine))
        |= movesParser definitions
        |. keywordEnd
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. oneOf [ token (Token "\n" (Expected EEndOfLine)), end (Expected EEndOfInput) ]


moveParser : Definitions -> Parser (Move Expr)
moveParser definitions =
    succeed identity
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= oneOf
            [ repeatParser definitions
            , doMoveParser |> andThen (lookupDefinition definitions)
            ]


actualsParser : Parser (List Expr)
actualsParser =
    let
        helper result =
            oneOf
                [ succeed (\arg -> Loop (arg :: result))
                    |= exprParser
                    |. chompWhile (\c -> c == ' ' || c == '\t')
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


argsParser : Parser (List Argument)
argsParser =
    let
        helper result =
            oneOf
                [ succeed (\arg -> Loop (arg :: result))
                    |= argParser
                    |. chompWhile (\c -> c == ' ' || c == '\t')
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


argParser : Parser Argument
argParser =
    oneOf
        [ pileNameParser |> map (\n -> { name = n, kind = KindPile })
        , numberNameParser |> map (\n -> { name = n, kind = KindInt })
        ]


lookupDefinition : Definitions -> ( String, List Expr ) -> Parser (Move Expr)
lookupDefinition definitions ( moveName, actualArgs ) =
    case Dict.get moveName definitions of
        Nothing ->
            problem (UnknownMove moveName)

        Just ({ name, args } as d) ->
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


movesParser : Definitions -> Parser (List (Move Expr))
movesParser definitions =
    let
        helper result =
            oneOf
                [ succeed (\cmd -> Loop (cmd :: result))
                    |= moveParser definitions
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


definitionParser : Definitions -> Parser MoveDefinition
definitionParser definitions =
    succeed (\name args moves -> { name = name, movesOrPrimitive = Moves moves, args = args })
        |. keywordDef
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= (moveNameParser
                |> andThen
                    (\n ->
                        if Dict.member n definitions then
                            problem (DuplicateDefinition n)

                        else
                            succeed n
                    )
           )
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= argsParser
        |. token (Token "\n" (Expected EEndOfLine))
        |= movesParser definitions
        |. keywordEnd
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. oneOf [ token (Token "\n" (Expected EEndOfLine)), end (Expected EEndOfInput) ]


definitionsAndMoves : Dict String MoveDefinition -> Parser ( Definitions, List (Move Expr) )
definitionsAndMoves primitives =
    definitionsParser primitives
        |> andThen
            (\defs ->
                movesParser defs
                    |> map (\moves -> ( defs, moves ))
            )


parser : Dict String MoveDefinition -> Parser { definitions : Definitions, moves : List (Move ExprValue) }
parser primitives =
    definitionsAndMoves primitives
        |> andThen
            (\( defs, moves ) ->
                case Move.substituteArguments [] moves of
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

                EInt ->
                    "a number (e.g. 3)"

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

        relevantLineAndPlace row col =
            case List.Extra.getAt (row - 1) (String.lines text) of
                Nothing ->
                    "THIS SHOULD NOT HAPPEN"

                Just line ->
                    String.fromInt row ++ "\n" ++ line ++ "\n" ++ String.repeat (col - 1) " " ++ "^\n"

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
    -- We always only deal with one problematic location at the time.
    case gatherDeadEndsByLocation deadEnds of
        [] ->
            ""

        de :: _ ->
            deadEndToString de


parseMoves : Definitions -> String -> Result String { moves : List (Move ExprValue), definitions : Definitions }
parseMoves primitives text =
    case run (parser primitives |. end (Expected EEndOfInput)) text of
        Ok m ->
            Ok m

        Err deadEnds ->
            Err (deadEndsToString text deadEnds)
