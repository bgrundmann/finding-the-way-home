module Cardician exposing (Cardician, andThen, andThenWithError, compose, cutOff, fail, faro, perform, put, return, take, takeEmptyOk)

import Card exposing (Card, Pile)
import Image exposing (Image, PileName)
import List
import List.Extra exposing (interweave, splitAt)
import Result


{-| A Cardician changes the world and computes something else or fails terribly...
Or with other words a State + Error Monad where the state is the Image
-}
type alias Cardician a =
    Image -> ( Result String a, Image )


return : a -> Cardician a
return x =
    \world -> ( Ok x, world )


fail : String -> Cardician a
fail msg =
    \world -> ( Err msg, world )


compose : Cardician a -> Cardician () -> Cardician a
compose c2 c1 =
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


{-| Take the given pile.
-}
take : PileName -> Cardician Pile
take pileName =
    \world ->
        case List.partition (\( n, v ) -> n == pileName) world of
            ( [], _ ) ->
                ( Err ("No pile called " ++ pileName), world )

            ( ( _, x ) :: _, newWorld ) ->
                ( Ok x, newWorld )


{-| Take the given pile. Or a pile of 0 cards, if no such pile exists.
-}
takeEmptyOk : PileName -> Cardician Pile
takeEmptyOk pileName =
    take pileName
        |> andThenWithError
            (\pileOrError ->
                case pileOrError of
                    Err e ->
                        return []

                    Ok pile ->
                        return pile
            )


replace : PileName -> Pile -> Cardician ()
replace pileName pile =
    \image -> ( Ok (), Image.update pileName (\_ -> Just pile) image )


{-| Put cards on top of given pile (or create a new pile if no pile exists)
-}
put : PileName -> Pile -> Cardician ()
put pileName cards =
    takeEmptyOk pileName
        |> andThen
            (\alreadyThere ->
                replace pileName (cards ++ alreadyThere)
            )


{-| Cut off the top N cards, leaving the rest.
-}
cutOff : Int -> PileName -> Cardician Pile
cutOff n pileName =
    take pileName
        |> andThen
            (\cards ->
                let
                    ( topHalf, lowerHalf ) =
                        splitAt n cards

                    actualLen =
                        List.length topHalf
                in
                if actualLen < n then
                    fail ("Only " ++ String.fromInt actualLen ++ " cards in pile " ++ pileName ++ " , wanted to cut off " ++ String.fromInt n)

                else
                    replace pileName lowerHalf
                        |> andThen (\() -> return topHalf)
            )


{-| Faro packet1 into packet2, starting at the top, such that packet1 card is the new top card.
Both packets to not need to be of the same length.
-}
faro : Pile -> Pile -> Cardician Pile
faro pile1 pile2 =
    return (interweave pile1 pile2)
