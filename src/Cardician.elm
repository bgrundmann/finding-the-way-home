module Cardician exposing (Cardician, andThen, andThenWithError, compose, fail, get, getOrEmpty, perform, put, return)

import Card exposing (Card, Pile)
import Image exposing (Image, PileName)
import List
import Result



-- A Cardician changes the world and computes something else or fails terribly...
-- Or with other words a State + Error Monad where the state is the Image


type alias Cardician a =
    Image -> ( Result String a, Image )


return : a -> Cardician a
return x =
    \world -> ( Ok x, world )


fail : String -> Cardician a
fail msg =
    \world -> ( Err msg, world )


compose : Cardician () -> Cardician a -> Cardician a
compose c1 c2 =
    c1
        |> andThen (\() -> c2)


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


perform : Cardician a -> Image -> ( Result String a, Image )
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


getOrEmpty : PileName -> Cardician Pile
getOrEmpty pileName =
    get pileName
        |> andThenWithError
            (\pileOrError ->
                case pileOrError of
                    Err e ->
                        return []

                    Ok pile ->
                        return pile
            )


put : PileName -> Pile -> Cardician ()
put pileName pile =
    \world ->
        let
            loop res l =
                case l of
                    [] ->
                        List.reverse (( pileName, pile ) :: res)

                    ( pN, v ) :: ls ->
                        if pN == pileName then
                            List.reverse (( pN, pile ) :: res) ++ ls

                        else
                            loop (( pN, v ) :: res) ls
        in
        ( Ok (), loop [] world )
