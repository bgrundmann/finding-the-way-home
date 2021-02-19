module MoveParserTests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Move exposing (ArgumentKind(..), Expr(..), ExprValue(..), Move(..), MoveDefinition, UserDefinedOrPrimitive(..))
import MoveParseError exposing (MoveParseError)
import MoveParser
import Primitives exposing (primitiveCut, primitiveTurnover, primitives)
import Tamariz
import Test exposing (..)


parseOk :
    List MoveDefinition
    -> List Move
    -> Result MoveParseError { definitions : List MoveDefinition, moves : List Move }
    -> Expectation
parseOk expectedDefinitions expectedMoves result =
    case result of
        Err err ->
            Expect.fail ("Expected ok got parse error:\n" ++ Debug.toString err)

        Ok res ->
            Expect.all
                [ Expect.equalLists expectedDefinitions << .definitions
                , Expect.equalLists expectedMoves << .moves
                ]
                res


parseShouldFail :
    String
    -> Result MoveParseError { definitions : List MoveDefinition, moves : List Move }
    -> Expectation
parseShouldFail expectedError result =
    case result of
        Err err ->
            Expect.equal expectedError (Debug.toString err)

        Ok res ->
            Expect.fail ("Expected parsing to fail. But got: " ++ Debug.toString res)


parseFailureTests : Test
parseFailureTests =
    describe "parse failures"
        [ test "repeat needs a integer" <|
            \() ->
                MoveParser.parseMoves primitives "repeat deck\nend"
                    |> parseShouldFail "[{ col = 8, contextStack = [], problem = Expected EInt, row = 1 },{ col = 8, contextStack = [], problem = Expected ENumberName, row = 1 }]"
        ]


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
                    [ Move.Repeat
                        (ExprValue (Int 4))
                        [ Move.Do
                            primitiveCut
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
                    , identifier = Move.makeIdentifier "deal" [ KindPile, KindPile ]
                    , path = []
                    , body =
                        UserDefined
                            { definitions = []
                            , temporaryPiles = []
                            , moves =
                                [ Move.Do
                                    primitiveCut
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


defTemporary : Test
defTemporary =
    test "definition with a temporary" <|
        \() ->
            let
                expected =
                    { name = "studdeal"
                    , args = [ { kind = KindPile, name = "a" }, { kind = KindPile, name = "b" } ]
                    , doc = "Stud deal a card"
                    , identifier = Move.makeIdentifier "studdeal" [ KindPile, KindPile ]
                    , path = []
                    , body =
                        UserDefined
                            { definitions = []
                            , temporaryPiles = [ "t" ]
                            , moves =
                                [ Move.Do
                                    primitiveCut
                                    [ ExprValue (Move.Int 1)
                                    , ExprArgument { kind = KindPile, name = "a", ndx = 0, up = 0 }
                                    , ExprTemporaryPile { name = "t", ndx = 0, up = 0 }
                                    ]
                                , Move.Do
                                    primitiveTurnover
                                    [ ExprTemporaryPile { name = "t", ndx = 0, up = 0 }
                                    ]
                                , Move.Do
                                    primitiveCut
                                    [ ExprValue (Move.Int 1)
                                    , ExprTemporaryPile { name = "t", ndx = 0, up = 0 }
                                    , ExprArgument { kind = KindPile, name = "b", ndx = 1, up = 0 }
                                    ]
                                ]
                            }
                    }
            in
            MoveParser.parseMoves primitives "def studdeal a b\ndoc Stud deal a card\ntemp t\ncut 1 a t\nturnover t\ncut 1 t b\nend"
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
                    , identifier = Move.makeIdentifier "x" []
                    , path = [ "deal" ]
                    , body =
                        UserDefined
                            { definitions = []
                            , temporaryPiles = []
                            , moves =
                                [ Move.Do
                                    primitiveCut
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
                    , identifier = Move.makeIdentifier "deal" [ KindPile, KindPile ]
                    , path = []
                    , body =
                        UserDefined
                            { definitions =
                                [ nestedX
                                ]
                            , temporaryPiles = []
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
                    , identifier = Move.makeIdentifier "x" [ KindPile ]
                    , path = [ "deal" ]
                    , body =
                        UserDefined
                            { definitions = []
                            , temporaryPiles = []
                            , moves =
                                [ Move.Do
                                    primitiveCut
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
                    , identifier = Move.makeIdentifier "deal" [ KindPile, KindPile ]
                    , path = []
                    , body =
                        UserDefined
                            { definitions =
                                [ nestedX
                                ]
                            , temporaryPiles = []
                            , moves =
                                [ Move.Do
                                    nestedX
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
