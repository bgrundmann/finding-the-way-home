module MoveParser exposing (parseMoves)

import Array exposing (Array)
import Char
import Dict exposing (Dict)
import Image exposing (PileName)
import List
import List.Extra
import Move exposing (..)
import Parser.Advanced exposing ((|.), (|=), Step(..), Token(..), andThen, chompWhile, end, int, keyword, loop, map, oneOf, problem, run, succeed, token, variable)
import Set


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type alias Context =
    ()


type alias DeadEnd =
    Parser.Advanced.DeadEnd Context Problem


type Problem
    = ExpectedMoveName
    | UnknownMove String
    | ExpectedInt
    | ExpectedPileName
    | ExpectedEndOfInput
    | ExpectedDef
    | ExpectedEnd
    | ExpectedRepeat
    | ExpectedEndOfLine
    | DuplicateDefinition String
    | Problem String


type alias Definitions =
    Dict String MoveDefinition


keywords =
    Set.fromList [ "repeat", "end", "def" ]


moveNameParser : Parser String
moveNameParser =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '-', reserved = keywords, expecting = ExpectedMoveName }


exprParser : Parser Expr
exprParser =
    oneOf
        [ int ExpectedInt ExpectedInt |> map Int
        , variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '-', reserved = Set.empty, expecting = ExpectedPileName }
            |> map Pile
        ]
        |> map ExprValue


doMoveParser : Parser ( String, List Expr )
doMoveParser =
    succeed (\cmd args -> ( cmd, args ))
        |= moveNameParser
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= actualsParser
        |. oneOf [ token (Token "\n" ExpectedEndOfLine), end ExpectedEndOfInput ]


repeatParser : Definitions -> Parser (Move Expr)
repeatParser definitions =
    succeed (\n moves -> Repeat n moves)
        |. keyword (Token "repeat" ExpectedRepeat)
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= exprParser
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. token (Token "\n" ExpectedEndOfLine)
        |= movesParser definitions
        |. keyword (Token "end" ExpectedEnd)
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. oneOf [ token (Token "\n" ExpectedEndOfLine), end ExpectedEndOfInput ]


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
    succeed (\name moves -> { name = name, movesOrPrimitive = Moves moves, args = [] })
        |. keyword (Token "def" ExpectedDef)
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
        |. token (Token "\n" ExpectedEndOfLine)
        |= movesParser definitions
        |. keyword (Token "end" ExpectedEnd)
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. oneOf [ token (Token "\n" ExpectedEndOfLine), end ExpectedEndOfInput ]


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


deadEndsToString : String -> List DeadEnd -> String
deadEndsToString text deadEnds =
    let
        problemToString problem =
            case problem of
                UnknownMove n ->
                    "Don't know how to do '" ++ n ++ "'"

                ExpectedMoveName ->
                    "Expected a move (e.g. 'deal')"

                Problem msg ->
                    msg

                ExpectedRepeat ->
                    "Expected 'repeat'"

                ExpectedDef ->
                    "Expected 'def'"

                DuplicateDefinition d ->
                    "You already know how to '" ++ d ++ "'"

                ExpectedEnd ->
                    "Expected 'end'"

                ExpectedEndOfInput ->
                    "End of file expected"

                ExpectedInt ->
                    "Expected an int (e.g. 52)"

                ExpectedPileName ->
                    "Expected the name of a pile (e.g. deck, table, ...)"

                ExpectedEndOfLine ->
                    "Expected to see the next line."

        relevantLineAndPlace row col =
            case List.Extra.getAt (row - 1) (String.lines text) of
                Nothing ->
                    "THIS SHOULD NOT HAPPEN"

                Just line ->
                    line ++ "\n" ++ String.repeat (col - 1) " " ++ "^\n"

        deadEndToString { row, col, problem } =
            relevantLineAndPlace row col ++ problemToString problem
    in
    -- We always only deal with one problem at the time.
    case deadEnds of
        [] ->
            ""

        de :: _ ->
            deadEndToString de


parseMoves : Definitions -> String -> Result String { moves : List (Move ExprValue), definitions : Definitions }
parseMoves primitives text =
    case run (parser primitives |. end ExpectedEnd) text of
        Ok m ->
            Ok m

        Err deadEnds ->
            Err (deadEndsToString text deadEnds)
