module MoveParserTests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Move exposing (ArgumentKind(..), Expr(..), ExprValue(..), Move(..), MoveDefinition, UserDefinedOrPrimitive(..))
import MoveParser
import Primitives exposing (primitiveCut, primitiveTurnover, primitives)
import Tamariz
import Test exposing (..)


parseOk :
    List MoveDefinition
    -> List Move
    -> Result String { definitions : List MoveDefinition, moves : List Move }
    -> Expectation
parseOk expectedDefinitions expectedMoves result =
    case result of
        Err msg ->
            Expect.fail ("Expected ok got parse error:\n" ++ msg)

        Ok res ->
            Expect.all
                [ Expect.equalLists expectedDefinitions << .definitions
                , Expect.equalLists expectedMoves << .moves
                ]
                res


primitivesTests : Test
primitivesTests =
    describe "primitives"
        [ test "cut" <|
            \() ->
                MoveParser.parseMoves primitives "cut 1 deck table"
                    |> parseOk []
                        [ Move.Do primitiveCut
                            [ ExprValue (Int 1)
                            , ExprValue (Pile "deck")
                            , ExprValue (Pile "table")
                            ]
                        ]
        , test "turnover" <|
            \() ->
                MoveParser.parseMoves primitives "turnover deck"
                    |> parseOk [] [ Move.Do primitiveTurnover [ ExprValue (Pile "deck") ] ]
        ]


repeatTest : Test
repeatTest =
    test "repeat" <|
        \() ->
            MoveParser.parseMoves primitives "repeat 4\n  cut 1 deck table\n\n  turnover table\nend"
                |> parseOk []
                    [ Move.Repeat (ExprValue (Int 4))
                        [ Move.Do primitiveCut
                            [ ExprValue (Int 1)
                            , ExprValue (Pile "deck")
                            , ExprValue (Pile "table")
                            ]
                        , Move.Do primitiveTurnover [ ExprValue (Pile "table") ]
                        ]
                    ]


defParserTest : Test
defParserTest =
    test "definition" <|
        \() ->
            let
                expected =
                    { name = "deal"
                    , args = [ { kind = KindPile, name = "a" }, { kind = KindPile, name = "b" } ]
                    , doc = "Testing docs"
                    , body =
                        UserDefined
                            { definitions = []
                            , moves =
                                [ Move.Do primitiveCut
                                    [ ExprValue (Move.Int 1)
                                    , ExprArgument { kind = KindPile, name = "a", ndx = 0, up = 0 }
                                    , ExprArgument { kind = KindPile, name = "b", ndx = 1, up = 0 }
                                    ]
                                ]
                            }
                    }
            in
            MoveParser.parseMoves primitives "def deal a b\ndoc Testing docs\ncut 1 a b\nend"
                |> parseOk [ expected ] []


nestedDefUsingOuterTest : Test
nestedDefUsingOuterTest =
    test "nested def using outer def" <|
        \() ->
            let
                nestedX =
                    { name = "x"
                    , args = []
                    , doc = ""
                    , body =
                        UserDefined
                            { definitions = []
                            , moves =
                                [ Move.Do primitiveCut
                                    [ ExprValue (Move.Int 1)
                                    , ExprArgument { kind = KindPile, name = "a", ndx = 0, up = 1 }
                                    , ExprArgument { kind = KindPile, name = "b", ndx = 1, up = 1 }
                                    ]
                                ]
                            }
                    }

                expected =
                    { name = "deal"
                    , args = [ { kind = KindPile, name = "a" }, { kind = KindPile, name = "b" } ]
                    , doc = "Testing docs"
                    , body =
                        UserDefined
                            { definitions =
                                [ nestedX
                                ]
                            , moves =
                                [ Move.Do nestedX []
                                ]
                            }
                    }
            in
            MoveParser.parseMoves primitives "def deal a b\ndoc Testing docs\ndef x\n cut 1 a b\nend\nx\nend"
                |> parseOk [ expected ] []


nestedDefVariablesTest : Test
nestedDefVariablesTest =
    test "nested def variables" <|
        \() ->
            let
                nestedX =
                    { name = "x"
                    , args = [ { kind = KindPile, name = "a" } ]
                    , doc = ""
                    , body =
                        UserDefined
                            { definitions = []
                            , moves =
                                [ Move.Do primitiveCut
                                    [ ExprValue (Move.Int 1)
                                    , ExprArgument { kind = KindPile, name = "a", ndx = 0, up = 0 }
                                    , ExprArgument { kind = KindPile, name = "b", ndx = 1, up = 1 }
                                    ]
                                ]
                            }
                    }

                expected =
                    { name = "deal"
                    , args = [ { kind = KindPile, name = "a" }, { kind = KindPile, name = "b" } ]
                    , doc = ""
                    , body =
                        UserDefined
                            { definitions =
                                [ nestedX
                                ]
                            , moves =
                                [ Move.Do nestedX
                                    [ ExprArgument { kind = KindPile, name = "b", ndx = 1, up = 0 }
                                    ]
                                ]
                            }
                    }
            in
            MoveParser.parseMoves primitives "def deal a b\n\ndef x a\n cut 1 a b\nend\nx b\nend"
                |> parseOk [ expected ] []


bigTest : Test
bigTest =
    test "parsing tamariz does not fail" <|
        \() ->
            MoveParser.parseMoves primitives Tamariz.tamariz
                |> Expect.ok
