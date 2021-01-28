module Image exposing (Image, PileName, get, names, piles, renamePile, update, view, viewPile)

import Card
import Element exposing (Element, column, el, paragraph, spacing, text, textColumn)
import Element.Font as Font
import List
import List.Extra exposing (greedyGroupsOf)
import Pile exposing (Pile)



-- For a cardician at any given point the Image we present to the audience is just
-- piles of cards..
-- Each pile has a name


type alias PileName =
    String


type alias Image =
    List ( PileName, Pile )


names : Image -> List String
names i =
    List.map (\( n, _ ) -> n) i


piles : Image -> List ( String, Pile )
piles image =
    image


get : PileName -> Image -> Maybe Pile
get pileName image =
    case List.filter (\( n, _ ) -> n == pileName) image of
        [] ->
            Nothing

        ( _, x ) :: _ ->
            Just x


{-| Rename oldname to newname. Does nothing if no pile has oldname.
-}
renamePile : PileName -> PileName -> Image -> Image
renamePile oldName newName image =
    List.map
        (\( n, v ) ->
            if oldName == n then
                ( newName, v )

            else
                ( n, v )
        )
        image


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
                [ el [ Font.size 10, Element.centerX ] (text (String.fromInt num))
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
