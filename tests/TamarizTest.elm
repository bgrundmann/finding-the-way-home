module TamarizTest exposing (..)

import Dict
import Eval
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Move
import MoveParser
import Pile
import Primitives exposing (primitives)
import Tamariz
import Test exposing (..)



-- This is the big end to End test.


testEndToEnd : String -> { initial : String, final : String, moves : String, backwards : Bool } -> Test
testEndToEnd label { initial, final, moves, backwards } =
    test label <|
        \() ->
            let
                parseResult =
                    Pile.fromString initial
                        |> Result.andThen
                            (\initialPile ->
                                Pile.fromString final
                                    |> Result.andThen
                                        (\finalPile ->
                                            MoveParser.parseMoves primitives moves
                                                |> Result.map
                                                    (\parsedMoves ->
                                                        { initialPile = initialPile
                                                        , finalPile = finalPile
                                                        , parsedMoves = parsedMoves.moves
                                                        }
                                                    )
                                        )
                            )
            in
            case parseResult of
                Err _ ->
                    Expect.fail "Parser failed"

                Ok { initialPile, finalPile, parsedMoves } ->
                    let
                        expectedFinalImage =
                            [ ( "deck", finalPile ) ]

                        initialImage =
                            [ ( "deck", initialPile ) ]

                        movesToApply =
                            if backwards then
                                Move.backwardsMoves parsedMoves

                            else
                                parsedMoves

                        result =
                            Eval.eval initialImage movesToApply
                    in
                    case result.error of
                        Just message ->
                            Expect.fail message

                        Nothing ->
                            Expect.equal expectedFinalImage result.lastImage


mnemonica : String
mnemonica =
    """4C, 2H, 7D, 3C, 4H, 6D, AS, 5H, 9S, 2S, QH, 3D, QC,
        8H, 6S, 5S, 9H, KC, 2D, JH, 3S, 8S, 6H, 10C, 5D, KD,
        2C, 3H, 8D, 5C, KS, JD, 8C, 10S, KH, JC, 7S, 10H, AD,
        4S, 7H, 4D, AC, 9C, JS, QD, 7C, QS, 10D, 6C, AH, 9D"""


suite : Test
suite =
    describe "End to End tests"
        [ testEndToEnd "tamariz"
            { initial = Pile.poker_deck |> Pile.toString
            , final = mnemonica
            , moves = Tamariz.tamariz
            , backwards = False
            }
        , testEndToEnd "tamariz backwards"
            { initial = mnemonica
            , final = Pile.poker_deck |> Pile.toString
            , moves = Tamariz.tamariz
            , backwards = True
            }
        ]
