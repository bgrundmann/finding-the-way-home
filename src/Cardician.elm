module Cardician exposing (Cardician, Error, andThen, andThenWithError, compose, cutOff, fail, perform, put, return, take, takeEmptyOk)

import Image exposing (Image, PileName)
import List
import List.Extra exposing (splitAt)
import Pile exposing (Pile)


{-| A Cardician changes the world and computes something else or fails terribly...
Or with other words a Reader + State + Error Monad where the state is the Image,
and the Reader is for the environment
-}
type alias Cardician a =
    Image -> ( Result String a, Image )


type alias Error =
    { lastImage : Image
    , message : String
    }


return : a -> Cardician a
return x =
    \world -> ( Ok x, world )


fail : String -> Cardician a
fail msg =
    \world -> ( Err msg, world )


{-| If an Error happens, report the incoming Image and not any intermediate state.
-}
atomicErrorReporting : Cardician a -> Cardician a
atomicErrorReporting m =
    \image ->
        let
            ( res, i ) =
                m image
        in
        case res of
            Err _ ->
                ( res, image )

            Ok _ ->
                ( res, i )


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


perform : Cardician () -> Image -> Result Error Image
perform cardician image =
    let
        ( res, lastImage ) =
            cardician image
    in
    case res of
        Err msg ->
            Err { message = msg, lastImage = lastImage }

        Ok () ->
            Ok lastImage


{-| Take the given pile.
-}
take : PileName -> Cardician Pile
take pileName =
    \world ->
        case List.partition (\( n, _ ) -> n == pileName) world of
            ( [], _ ) ->
                fail ("No pile called " ++ pileName) world

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
                    Err _ ->
                        return []

                    Ok pile ->
                        return pile
            )


replace : PileName -> Pile -> Cardician ()
replace pileName pile =
    \image ->
        ( Ok ()
        , Image.update pileName
            (\_ ->
                case pile of
                    [] ->
                        Nothing

                    _ ->
                        Just pile
            )
            image
        )


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
    atomicErrorReporting
        (take pileName
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
        )
