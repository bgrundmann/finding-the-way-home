module TamarizTest exposing (..)

import Dict
import Eval
import EvalResult exposing (Problem(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Move exposing (ArgumentKind(..), Expr(..), ExprValue(..), Move(..), UserDefinedOrPrimitive(..))
import MoveParseError exposing (MoveParseError)
import MoveParser
import Pile
import Primitives exposing (primitiveCut, primitives)
import Tamariz
import Test exposing (..)



-- This is the big end to End test.


testEndToEnd : String -> { initial : String, final : String, moves : String, backwards : Bool, expectEvalFailure : Maybe EvalResult.Problem } -> Test
testEndToEnd label { initial, final, moves, backwards, expectEvalFailure } =
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
                                                |> Result.mapError Debug.toString
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
                Err err ->
                    Expect.fail ("Parser failed: " ++ err)

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
                    case expectEvalFailure of
                        Just expectedProblem ->
                            case result.error of
                                Nothing ->
                                    Expect.fail "Expected evaluation to fail.  But it did not!"

                                Just { problem } ->
                                    Expect.equal expectedProblem problem

                        Nothing ->
                            case result.error of
                                Just { problem } ->
                                    Expect.fail (Debug.toString problem)

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
            , expectEvalFailure = Nothing
            }
        , testEndToEnd "tamariz backwards"
            { initial = mnemonica
            , final = Pile.poker_deck |> Pile.toString
            , moves = Tamariz.tamariz
            , backwards = True
            , expectEvalFailure = Nothing
            }
        , testEndToEnd "Cutting more cards than available causes a runtime error"
            { initial = Pile.poker_deck |> Pile.toString
            , final = Pile.poker_deck |> Pile.toString
            , moves = """cut 53 deck table"""
            , backwards = False
            , expectEvalFailure = Just (NotEnoughCards { expected = 53, got = 52, inPile = "deck" })
            }
        , testEndToEnd "Temporary piles that are not empty cause a runtime error"
            { initial = Pile.poker_deck |> Pile.toString
            , final = Pile.poker_deck |> Pile.toString
            , moves = """def bad deck
                           temp t
                           cut 1 deck t
                         end
                         bad deck"""
            , backwards = False
            , expectEvalFailure =
                Just
                    (TemporaryPileNotEmpty
                        { names = [ "t" ]
                        , moveDefinition =
                            { name = "bad"
                            , args = [ { kind = KindPile, name = "deck" } ]
                            , doc = ""
                            , body =
                                UserDefined
                                    { temporaryPiles = [ "t" ]
                                    , definitions = []
                                    , moves =
                                        [ Do { row = 3 }
                                            primitiveCut
                                            [ ExprValue (Int 1)
                                            , ExprArgument { kind = KindPile, name = "deck", ndx = 0, up = 0 }
                                            , ExprTemporaryPile { name = "t", ndx = 0, up = 0 }
                                            ]
                                        ]
                                    }
                            }
                        }
                    )
            }
        ]
