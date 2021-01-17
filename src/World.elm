module World exposing (..)

import Card exposing (Card, Pile)
import Html exposing (..)
import List



-- For a cardician the world is just piles of cards.
-- Each pile has a name


type alias PileName =
    String


type alias World =
    List ( PileName, Pile )


view : World -> Html msg
view world =
    div []
        (List.map (\( name, pile ) -> div [] [ text name, div [] (List.map Card.view pile) ]) world)



-- A Cardician changes the world and computes something else
-- Or with other words a State Monad where the state is the World


type alias Cardician a =
    World -> ( a, World )


return : a -> Cardician a
return x =
    \world -> ( x, world )


andThen : (a -> Cardician b) -> Cardician a -> Cardician b
andThen f m =
    \world ->
        let
            ( res, next_world ) =
                m world
        in
        f res next_world


perform : Cardician a -> World -> ( a, World )
perform cardician world =
    cardician world


get : PileName -> Cardician Pile
get pileName =
    \world ->
        case
            List.filterMap
                (\( n, v ) ->
                    if n == pileName then
                        Just v

                    else
                        Nothing
                )
                world
        of
            [] ->
                ( [], world )

            x :: _ ->
                ( x, world )


put : PileName -> Pile -> Cardician ()
put pileName pile =
    \world ->
        let
            loop res l =
                case l of
                    [] ->
                        List.reverse (( pileName, pile ) :: l)

                    ( pN, v ) :: ls ->
                        if pN == pileName then
                            List.reverse (( pN, pile ) :: res) ++ ls

                        else
                            loop (( pN, v ) :: res) ls
        in
        ( (), loop [] world )
