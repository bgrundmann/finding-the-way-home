module Image exposing (..)

import Card exposing (Card, Pile)
import Element exposing (Element, column, el, paragraph, row, text, textColumn)
import List
import List.Extra exposing (greedyGroupsOf)



-- For a cardician at any given point the Image we present to the audience is just
-- piles of cards..
-- Each pile has a name


type alias PileName =
    String


type alias Image =
    List ( PileName, Pile )


viewPile : Pile -> Element msg
viewPile pile =
    textColumn []
        (greedyGroupsOf 13 pile
            |> List.map (\p -> paragraph [] (List.map Card.view p))
        )


view : Image -> Element msg
view world =
    column [ Element.height Element.fill, Element.width Element.fill, Element.spacing 10 ]
        (List.map (\( name, pile ) -> column [] [ text name, viewPile pile ]) world)
