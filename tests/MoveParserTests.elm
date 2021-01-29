module MoveParserTests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Move
import MoveParser
import Primitives exposing (primitiveCut, primitiveTurnover, primitives)
import Tamariz
import Test exposing (..)


parseOk expected result =
    case result of
        Err _ ->
            Expect.fail "Expected ok got Err"

        Ok { definitions, moves } ->
            Expect.equalLists expected moves


primitivesTests : Test
primitivesTests =
    describe "primitives"
        [ test "cut" <|
            \() ->
                MoveParser.parseMoves primitives "cut 1 deck table"
                    |> parseOk [ Move.Do primitiveCut [ Move.Int 1, Move.Pile "deck", Move.Pile "table" ] ]
        , test "turnover" <|
            \() ->
                MoveParser.parseMoves primitives "turnover deck"
                    |> parseOk [ Move.Do primitiveTurnover [ Move.Pile "deck" ] ]
        ]


repeatTest : Test
repeatTest =
    test "repeat" <|
        \() ->
            MoveParser.parseMoves primitives "repeat 4\n  cut 1 deck table\n\n  turnover table\nend"
                |> parseOk
                    [ Move.Repeat (Move.Int 4)
                        [ Move.Do primitiveCut [ Move.Int 1, Move.Pile "deck", Move.Pile "table" ]
                        , Move.Do primitiveTurnover [ Move.Pile "table" ]
                        ]
                    ]


bigTest : Test
bigTest =
    test "parsing tamariz does not fail" <|
        \() ->
            MoveParser.parseMoves primitives Tamariz.tamariz
                |> Expect.ok


suite : Test
suite =
    describe "Test the MoveParser"
        [ primitivesTests
        ]
