module Image exposing (Image, PileName, update, view)

import Card exposing (Card, Pile)
import Element exposing (Element, column, el, fill, height, paragraph, row, spacing, text, textColumn, width)
import Element.Font as Font
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
    let
        numberedPile =
            List.indexedMap (\n c -> ( n + 1, c )) pile

        viewNumberedCard ( num, c ) =
            column []
                [ el [ Font.size 10 ] (text (String.fromInt num))
                , Card.view c
                ]
    in
    textColumn []
        (greedyGroupsOf 13 numberedPile
            |> List.map (\p -> paragraph [] (List.map viewNumberedCard p))
        )


view : (String -> Element msg) -> Image -> Element msg
view viewPileName world =
    column [ spacing 10 ]
        (List.map (\( name, pile ) -> column [] [ viewPileName name, viewPile pile ]) world)
