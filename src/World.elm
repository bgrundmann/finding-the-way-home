module World exposing (..)

import Card exposing (Card, Pile)
import Element exposing (Element, el, text, row, column, paragraph, textColumn)
import List
import List.Extra exposing (greedyGroupsOf)


-- For a cardician the world is just piles of cards.
-- Each pile has a name


type alias PileName =
    String


type alias World =
    List ( PileName, Pile )

viewPile : Pile -> Element msg
viewPile pile =
  textColumn []
    (greedyGroupsOf 13 pile
     |> List.map (\p -> paragraph [] (List.map Card.view p)))

view : World -> Element msg
view world =
    column []
        (List.map (\( name, pile ) -> column [] [ text name, viewPile pile]) world)



