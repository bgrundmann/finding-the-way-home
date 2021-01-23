module Image exposing (..)

import Card exposing (Card, Pile)
import Element exposing (Element, column, el, fill, height, paragraph, row, spacing, text, textColumn, width)
import List
import List.Extra exposing (greedyGroupsOf)



-- For a cardician at any given point the Image we present to the audience is just
-- piles of cards..
-- Each pile has a name


type alias PileName =
    String


type alias Image =
    List ( PileName, Pile )


update : PileName -> (Maybe Pile -> Maybe Pile) -> Image -> Image
update pileName f image =
    let
        loop res l =
            case l of
                [] ->
                    case f Nothing of
                        Nothing ->
                            List.reverse res

                        Just newPile ->
                            List.reverse res ++ [ ( pileName, newPile ) ]

                ( pN, v ) :: ls ->
                    if pN == pileName then
                        case f (Just v) of
                            Nothing ->
                                List.reverse res ++ ls

                            Just newPile ->
                                List.reverse (( pN, newPile ) :: res) ++ ls

                    else
                        loop (( pN, v ) :: res) ls
    in
    loop [] image


viewPile : Pile -> Element msg
viewPile pile =
    textColumn []
        (greedyGroupsOf 13 pile
            |> List.map (\p -> paragraph [] (List.map Card.view p))
        )


view : (String -> Element msg) -> Image -> Element msg
view viewPileName world =
    column [ height fill, width fill, spacing 10 ]
        (List.map (\( name, pile ) -> column [ height fill, width fill ] [ viewPileName name, viewPile pile ]) world)
