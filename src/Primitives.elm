module Primitives exposing (eval, primitiveCut, primitiveTurnover, primitives)

import Card
import Dict exposing (Dict)
import Image exposing (Image, PileName)
import List.Extra
import Move
    exposing
        ( Argument
        , ArgumentKind(..)
        , Expr(..)
        , ExprValue(..)
        , Move(..)
        , MoveDefinition
        , Primitive(..)
        , UserDefinedOrPrimitive(..)
        )
import Pile exposing (Pile)



--- Primitives


turnover : Pile -> Pile
turnover pile =
    List.reverse (List.map Card.turnover pile)


decodeActuals :
    { turnover : PileName -> a, cut : Int -> PileName -> PileName -> a, decodingError : Primitive -> a }
    -> Primitive
    -> List ExprValue
    -> a
decodeActuals handlers p actuals =
    case ( p, actuals ) of
        ( Turnover, [ Pile name ] ) ->
            handlers.turnover name

        ( Cut, [ Int n, Pile from, Pile to ] ) ->
            handlers.cut n from to

        ( _, _ ) ->
            handlers.decodingError p


eval : Image -> Primitive -> List ExprValue -> { lastImage : Image, error : Maybe String }
eval image =
    decodeActuals
        { turnover =
            \name ->
                let
                    ( pile, newImage ) =
                        Image.take name image
                in
                case pile of
                    Nothing ->
                        { lastImage = image, error = Just ("No pile called " ++ name) }

                    Just p ->
                        { lastImage = Image.put name (turnover p) newImage, error = Nothing }
        , cut =
            \n from to ->
                if n == 0 then
                    { lastImage = image, error = Nothing }

                else
                    let
                        ( pile, imageAfterTake ) =
                            Image.take from image
                    in
                    case pile of
                        Nothing ->
                            { lastImage = image, error = Just ("No pile called " ++ from) }

                        Just cards ->
                            let
                                ( topHalf, lowerHalf ) =
                                    List.Extra.splitAt n cards

                                actualLen =
                                    List.length topHalf
                            in
                            if actualLen < n then
                                { lastImage = image
                                , error = Just ("Only " ++ String.fromInt actualLen ++ " cards in pile " ++ from ++ " , wanted to cut off " ++ String.fromInt n)
                                }

                            else
                                { lastImage =
                                    imageAfterTake
                                        |> Image.put from lowerHalf
                                        |> Image.put to topHalf
                                , error = Nothing
                                }
        , decodingError = \p -> { lastImage = image, error = Just "Internal error: type checker failed" }
        }


intArg : String -> Argument
intArg name =
    { name = name, kind = KindInt }


pileArg : String -> Argument
pileArg name =
    { name = name, kind = KindPile }


primitive : String -> List Argument -> Primitive -> MoveDefinition
primitive name args p =
    { name = name, args = args, body = Primitive p, doc = "" }


primitiveTurnover : MoveDefinition
primitiveTurnover =
    primitive "turnover" [ pileArg "pile" ] Turnover


primitiveCut : MoveDefinition
primitiveCut =
    primitive "cut" [ intArg "N", pileArg "from", pileArg "to" ] Cut


primitives : Dict String MoveDefinition
primitives =
    [ primitiveCut
    , primitiveTurnover
    ]
        |> List.map (\m -> ( m.name, m ))
        |> Dict.fromList
