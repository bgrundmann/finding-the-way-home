module World exposing (..)

import List

import Html exposing (..)

import Card exposing (Card, Pile)

-- For a card magician the world is just piles of cards.
-- Each pile has a name
type alias World = List (String, Pile)


view: World -> Html msg
view world =
   div [] (
    List.map (\(name, pile) -> div [] [text name, div [] (List.map Card.view pile)]) world)
