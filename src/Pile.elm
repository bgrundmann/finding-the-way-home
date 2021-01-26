module Pile exposing (Pile, fromString, pileParser, poker_deck, toString)

import Card exposing (Card, Suit(..), Value(..), all_suits, all_values, card, cardParser)
import List.Extra
import Parser exposing ((|.), (|=), Parser, Step(..), loop, map, oneOf, spaces, succeed)


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


separatedByComma : Parser a -> Parser (List a)
separatedByComma elem =
    let
        helper res =
            oneOf
                [ succeed (\r -> Loop (r :: res))
                    |. spaces
                    |. Parser.token ","
                    |. spaces
                    |= elem
                , succeed () |> map (\_ -> Done (List.reverse res))
                ]
    in
    succeed (\x xs -> x :: xs)
        |= elem
        |= loop [] helper


pileParser : Parser Pile
pileParser =
    separatedByComma Card.cardParser


fromString : String -> Result String Pile
fromString s =
    case Parser.run (pileParser |. Parser.end) s of
        Err _ ->
            Err "syntax error"

        Ok p ->
            Ok p


toString : Pile -> String
toString pile =
    List.map Card.toString pile
        |> List.Extra.greedyGroupsOf 13
        |> List.map (String.join ", ")
        |> String.join ",\n"
