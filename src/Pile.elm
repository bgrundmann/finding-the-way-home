module Pile exposing (Pile, poker_deck)

import Card exposing (Card, Suit(..), Value(..), all_suits, all_values, card, cardParser)


type alias Pile =
    List Card


poker_deck : Pile
poker_deck =
    let
        all suit =
            List.map (\v -> card v suit) all_values
    in
    all Clubs
        ++ all Diamonds
        ++ (all Hearts |> List.reverse)
        ++ (all Spades |> List.reverse)
