module CardTest exposing (..)

import Card
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Pile
import Test exposing (..)


toFromStringBijection : Test
toFromStringBijection =
    test "to <-> from String bijection" <|
        \() ->
            let
                stringsAndBack =
                    List.map Card.toString Pile.poker_deck
                        |> List.filterMap Card.fromString
            in
            Expect.equalLists stringsAndBack Pile.poker_deck


toStringExamples : Test
toStringExamples =
    describe "Some concrete Examples to describe the exact semantics"
        [ test "hidden / visible" <|
            \() ->
                Expect.equal
                    (Card.blank
                        |> Card.withVisible (Card.Face ( Card.Ace, Card.Spades ))
                        |> Card.withHidden (Card.Back Card.Blue)
                        |> Card.toString
                    )
                    "B/AS"
        , test "Red backed, face down card has a shortcut syntax" <|
            \() ->
                Expect.equal (Card.card Card.Ace Card.Spades |> Card.toString)
                    "AS"
        , test "Red Backed, face down card long syntax can be read" <|
            \() ->
                Expect.equal (Card.fromString "AS/R") (Just (Card.card Card.Ace Card.Spades))
        , test "Blue Backed, face down card long syntax can be read" <|
            \() ->
                Expect.equal
                    (Card.fromString "AS/B")
                    (Just (Card.card Card.Ace Card.Spades |> Card.withVisible (Card.Back Card.Blue)))
        , test
            "Red backed, face up card"
          <|
            \() ->
                Expect.equal (Card.card Card.Ace Card.Spades |> Card.turnover |> Card.toString) "R/AS"
        ]
