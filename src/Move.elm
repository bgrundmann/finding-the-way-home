module Move exposing (Move(..), parseMoves)

import Char
import Image exposing (PileName)
import List
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
    | ExpectedEnd
    | ExpectedRepeat
    | ExpectedEndOfLine
    | Problem String


type Move
    = Cut { n : Int, pile : PileName, to : PileName }
    | Repeat Int (List Move)
    | Turnover PileName


type Argument
    = Int Int
    | Pile PileName


keywords =
    Set.fromList [ "repeat", "end", "def" ]


primitiveMoveParser : Parser ( String, List Argument )
primitiveMoveParser =
    succeed (\cmd args -> ( cmd, args ))
        |= variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '-', reserved = keywords, expecting = ExpectedMoveName }
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= argsParser
        |. oneOf [ token (Token "\n" ExpectedEndOfLine), end ExpectedEndOfInput ]


repeatParser =
    succeed (\n moves -> Repeat n moves)
        |. keyword (Token "repeat" ExpectedRepeat)
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= int ExpectedInt ExpectedInt
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. token (Token "\n" ExpectedEndOfLine)
        |= movesParser
        |. keyword (Token "end" ExpectedEnd)
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |. oneOf [ token (Token "\n" ExpectedEndOfLine), end ExpectedEndOfInput ]


moveParser =
    succeed identity
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= oneOf
            [ repeatParser
            , primitiveMoveParser |> andThen recognizeBuiltins
            ]


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

        argParser =
            oneOf
                [ int ExpectedInt ExpectedInt |> map Int
                , variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '-', reserved = Set.empty, expecting = ExpectedPileName }
                    |> map Pile
                ]
    in
    loop [] helper


recognizeBuiltins : ( String, List Argument ) -> Parser Move
recognizeBuiltins ( cmd, args ) =
    case ( cmd, args ) of
        ( "cut", [ Int n, Pile pile, Pile to ] ) ->
            Cut { n = n, pile = pile, to = to }
                |> succeed

        ( "cut", _ ) ->
            Problem "cut <number> <pile> <to-pile>"
                |> problem

        ( "turnover", [ Pile name ] ) ->
            Turnover name
                |> succeed

        ( "turnover", _ ) ->
            Problem "turnover <pile>"
                |> problem

        ( other, _ ) ->
            UnknownMove other
                |> problem


movesParser : Parser (List Move)
movesParser =
    let
        helper result =
            oneOf
                [ succeed (\cmd -> Loop (cmd :: result))
                    |= moveParser
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


parser : Parser (List Move)
parser =
    movesParser


deadEndsToString : String -> List DeadEnd -> String
deadEndsToString text s =
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

                ExpectedEnd ->
                    "Expected 'end'"

                ExpectedEndOfInput ->
                    "Expected that we were done, but there was more and I don't recognize what it is."

                ExpectedInt ->
                    "Expected an int (e.g. 52)"

                ExpectedPileName ->
                    "Expected the name of a pile (e.g. deck, table, ...)"

                ExpectedEndOfLine ->
                    "Expected to see the end of that line, but there was more and I don't know what to do with it."

        deadEndToString { row, col, problem } =
            String.fromInt row ++ "x" ++ String.fromInt col ++ problemToString problem
    in
    String.join "\n" (List.map deadEndToString s)


parseMoves : String -> Result String (List Move)
parseMoves text =
    case run (parser |. end ExpectedEnd) text of
        Ok m ->
            Ok m

        Err deadEnds ->
            Err (deadEndsToString text deadEnds)
