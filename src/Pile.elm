module Pile exposing
    ( Pile
    , fromString
    , pileParser
    , poker_deck
    , sort
    , toString
    , turnover
    , view
    )

import Card exposing (Card, Suit(..), Value(..), all_values, card, cardParser)
import Element exposing (Element, column, el, fill, height, padding, paragraph, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import List.Extra
import Palette
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


{-| Sort a pile so that all cards that there are appear in the same order
as new deck order.
-}
sort : Pile -> Pile
sort p =
    let
        -- First add the numbers as per new deck order
        -- This algorithm is quadratric but 52 * 52 is still not that big
        pileLen =
            List.length p

        numberedPile =
            List.indexedMap
                (\originalPosition card ->
                    case List.Extra.elemIndex card poker_deck of
                        Just ndx ->
                            ( ndx, card )

                        Nothing ->
                            -- Not found try turning the card over
                            case List.Extra.elemIndex (Card.turnover card) poker_deck of
                                Just ndx ->
                                    ( ndx, card )

                                Nothing ->
                                    -- Still not found, keep order relativ to other
                                    -- not found cards but before all sorted cards
                                    ( -pileLen + originalPosition, card )
                )
                p
    in
    List.sortBy Tuple.first numberedPile
        |> List.map Tuple.second


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


view : (Int -> Card -> Bool) -> Maybe (Int -> Card -> msg) -> Pile -> Element msg
view isSelected maybeOnClick pile =
    -- By default we show the hidden side in big and the visible side in small
    -- the assumption being that most of the time the deck will be face down
    let
        numberedPile =
            List.indexedMap (\n c -> ( n, c )) pile

        viewNumberedCard ( num, c ) =
            let
                maybeSelectedBorder =
                    if isSelected num c then
                        [ Border.width 2, padding 1, Border.rounded 3, Border.color Palette.greenBook ]

                    else
                        [ padding 3 ]

                cardEl =
                    -- TODO: Figure out how to do the -6 in a more elegant /
                    -- safe way.
                    Element.column
                        (Element.spacing -6 :: maybeSelectedBorder)
                        [ row [ width fill, Element.paddingXY 4 0 ]
                            [ el [ Font.variant Font.tabularNumbers, Font.size 12, width fill ]
                                (text (String.fromInt <| num + 1))
                            , el [ Font.variant Font.tabularNumbers, Font.size 12 ]
                                (text (Card.toString c))
                            ]
                        , el [ Font.size 64 ] (Card.view c)
                        ]
            in
            case maybeOnClick of
                Nothing ->
                    cardEl

                Just onClick ->
                    Input.button [ Element.focused [] ] { label = cardEl, onPress = Just (onClick num c) }
    in
    Element.wrappedRow [ Element.spacing 1 ] (List.map viewNumberedCard numberedPile)


turnover : Pile -> Pile
turnover pile =
    List.reverse (List.map Card.turnover pile)



{-
   textColumn [ Element.spacing 5 ]
       (List.Extra.greedyGroupsOf 13 numberedPile
           |> List.map (\p -> paragraph [ Element.spacing 5 ] (List.map viewNumberedCard p))
       )
-}
