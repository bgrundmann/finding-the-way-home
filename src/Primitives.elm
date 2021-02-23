module Primitives exposing (eval, primitiveCut, primitiveTurnover, primitives)

import EvalResult exposing (EvalResult, EvalTrace, Problem(..), reportError)
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
import MoveLibrary exposing (MoveLibrary)
import Pile exposing (Pile)



--- Primitives


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


eval : Image -> Int -> EvalTrace -> Primitive -> List ExprValue -> EvalResult
eval image steps trace =
    decodeActuals
        { turnover =
            \name ->
                let
                    ( pile, newImage ) =
                        Image.take name image
                in
                case pile of
                    Nothing ->
                        reportError image steps trace (NoSuchPile { name = name })

                    Just p ->
                        { lastImage = Image.put name (Pile.turnover p) newImage
                        , steps = steps
                        , trace = trace
                        , error = Nothing
                        }
        , cut =
            \n from to ->
                if n == 0 then
                    { lastImage = image
                    , steps = steps
                    , error = Nothing
                    , trace = trace
                    }

                else
                    let
                        ( pile, imageAfterTake ) =
                            Image.take from image
                    in
                    case pile of
                        Nothing ->
                            reportError image steps trace (NoSuchPile { name = from })

                        Just cards ->
                            let
                                ( topHalf, lowerHalf ) =
                                    List.Extra.splitAt n cards

                                actualLen =
                                    List.length topHalf
                            in
                            if actualLen < n then
                                reportError image
                                    steps
                                    trace
                                    (NotEnoughCards { expected = n, got = actualLen, inPile = from })

                            else
                                { lastImage =
                                    imageAfterTake
                                        |> Image.put from lowerHalf
                                        |> Image.put to topHalf
                                , error = Nothing
                                , steps = steps
                                , trace = trace
                                }
        , decodingError =
            \_ ->
                reportError image steps trace (Bug "type checker failed")
        }


intArg : String -> Argument
intArg name =
    { name = name, kind = KindInt }


pileArg : String -> Argument
pileArg name =
    { name = name, kind = KindPile }


primitive : String -> List Argument -> Primitive -> String -> MoveDefinition
primitive name args p doc =
    { name = name
    , args = args
    , body = Primitive p
    , doc = doc
    , path = []
    , identifier = Move.makeIdentifier name (List.map .kind args)
    }


primitiveTurnover : MoveDefinition
primitiveTurnover =
    primitive "turnover" [ pileArg "pile" ] Turnover "turnover pile (what was hidden becomes visible)"


primitiveCut : MoveDefinition
primitiveCut =
    primitive "cut" [ intArg "N", pileArg "a", pileArg "b" ] Cut "cut N cards from a to b"


primitives : MoveLibrary
primitives =
    [ primitiveCut
    , primitiveTurnover
    ]
        |> MoveLibrary.fromList
