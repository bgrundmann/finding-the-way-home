module Move exposing (Move(..), parseMoves)

import Char
import Image exposing (PileName)
import List
import Parser.Advanced exposing ((|.), (|=), Step(..), Token(..), andThen, chompWhile, end, int, keyword, loop, map, oneOf, problem, run, succeed, token, variable)
import Set



-- Core syntax
--
-- Primitives:
--   cut <n> <from-pile> <to-pile> # Move the top n cards of from-pile to to-pile (creating to-pile if it does not exist)
--
--   turnover <pile>
--
--   Inverses:
--   cut <n> <from-pile> <to-pile> -> cut <n> <to-pile> <from-pile>
--   turnover <pile> -> turnover <pile>
--
-- def deal pile1 pile2
--   cut 1 pile1 pile2
-- end
--
-- def studdeal pile1 pile2
--   with-new-pile temp
--     deal pile1 temp
--     turnover temp
--     deal temp pile2
--   end
-- end


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
    | ExpectedEnd
    | ExpectedEndOfLine
    | Problem String


type Move
    = Cut { n : Int, pile : PileName, to : PileName }
    | Faro { pile1 : PileName, pile2 : PileName, result : PileName }
    | Turnover PileName


type Argument
    = Int Int
    | Pile PileName


cmdParser : Parser ( String, List Argument )
cmdParser =
    succeed (\cmd args -> ( cmd, args ))
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '-', reserved = Set.empty, expecting = ExpectedMoveName }
        |. chompWhile (\c -> c == ' ' || c == '\t')
        |= argsParser
        |. oneOf [ token (Token "\n" ExpectedEndOfLine), end ExpectedEnd ]


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

        ( "faro", [ Pile pile1, Pile pile2, Pile result ] ) ->
            Faro { pile1 = pile1, pile2 = pile2, result = result }
                |> succeed

        ( "faro", _ ) ->
            Problem "faro <pile1> <pile2> <to-pile>"
                |> problem

        ( other, _ ) ->
            UnknownMove other
                |> problem


parser : Parser (List Move)
parser =
    let
        helper result =
            oneOf
                [ succeed (\cmd -> Loop (cmd :: result))
                    |= (cmdParser |> andThen recognizeBuiltins)
                , succeed () |> map (\() -> Done (List.reverse result))
                ]
    in
    loop [] helper


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

                ExpectedEnd ->
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
