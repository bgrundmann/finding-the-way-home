module Pile exposing (Pile, fromString, pileParser, poker_deck, toString, view)

import Card exposing (Card, Suit(..), Value(..), all_values, card, cardParser)
import Element exposing (Element, column, el, fill, paragraph, row, text, textColumn, width)
import Element.Font as Font
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


view : Pile -> Element msg
view pile =
    -- By default we show the hidden side in big and the visible side in small
    -- the assumption being that most of the time the deck will be face down
    let
        numberedPile =
            List.indexedMap (\n c -> ( n + 1, c )) pile

        viewNumberedCard ( num, c ) =
            -- TODO: Figure out how to do the -5 in a more elegant /
            -- safe way.
            Element.column [ Element.spacing -6 ]
                [ row [ width fill, Element.paddingXY 4 0 ]
                    [ el [ Font.variant Font.tabularNumbers, Font.size 15, width fill ] (text (String.fromInt num))
                    , el [ Font.size 26 ] (Card.view c)
                    ]
                , el [ Font.size 64 ] (Card.view (Card.turnover c))
                ]
    in
    paragraph [ Element.spacing 5 ] (List.map viewNumberedCard numberedPile)



{-
   textColumn [ Element.spacing 5 ]
       (List.Extra.greedyGroupsOf 13 numberedPile
           |> List.map (\p -> paragraph [ Element.spacing 5 ] (List.map viewNumberedCard p))
       )
-}
