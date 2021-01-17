module Cardician exposing (Cardician, andThen, andThenWithError, fail, get, perform, put, return)

import Card exposing (Card, Pile)
import List
import Result
import World exposing (PileName, World)



-- A Cardician changes the world and computes something else or fails terribly...
-- Or with other words a State + Error Monad where the state is the World


type alias Cardician a =
    World -> ( Result String a, World )


return : a -> Cardician a
return x =
    \world -> ( Ok x, world )


fail : String -> Cardician a
fail msg =
    \world -> ( Err msg, world )


andThen : (a -> Cardician b) -> Cardician a -> Cardician b
andThen f m =
    andThenWithError
        (\x ->
            case x of
                Err e ->
                    fail e

                Ok y ->
                    f y
        )
        m


andThenWithError : (Result String a -> Cardician b) -> Cardician a -> Cardician b
andThenWithError f m =
    \world ->
        let
            ( res, next_world ) =
                m world
        in
        f res next_world


perform : Cardician a -> World -> ( Result String a, World )
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
                ( Err ("No pile called " ++ pileName), world )

            x :: _ ->
                ( Ok x, world )


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
        ( Ok (), loop [] world )
