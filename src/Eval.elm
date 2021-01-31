module Eval exposing (eval)

import EvalResult exposing (EvalResult, reportError)
import Image exposing (Image)
import List.Extra
import Move exposing (Expr(..), ExprValue(..), Move(..), UserDefinedOrPrimitive(..))
import Primitives


type alias Env =
    List (List ExprValue)


evalWithEnv : Env -> Image -> Move -> EvalResult
evalWithEnv env image move =
    let
        replaceArgumentByValue e expr =
            case expr of
                ExprValue v ->
                    v

                ExprArgument { up, ndx } ->
                    let
                        value =
                            List.Extra.getAt up e
                                |> Maybe.andThen
                                    (\args ->
                                        List.Extra.getAt ndx args
                                    )
                    in
                    case value of
                        Nothing ->
                            Pile "INTERNAL ERROR"

                        Just v ->
                            v
    in
    case move of
        Repeat loc expr moves ->
            case replaceArgumentByValue env expr of
                Int times ->
                    let
                        helper n currentImage =
                            if n <= 0 then
                                { lastImage = currentImage, error = Nothing }

                            else
                                let
                                    result =
                                        evalListWithEnv env currentImage moves
                                in
                                case result.error of
                                    Nothing ->
                                        helper (n - 1) result.lastImage

                                    Just _ ->
                                        result
                    in
                    helper times image

                Pile _ ->
                    reportError image "INTERNAL ERROR: Type checker failed"

        Do loc { body } actuals ->
            let
                actualValues =
                    List.map (replaceArgumentByValue env) actuals
            in
            case body of
                Primitive p ->
                    Primitives.eval image p actualValues

                UserDefined { moves } ->
                    evalListWithEnv (actualValues :: env) image moves


evalListWithEnv : Env -> Image -> List Move -> EvalResult
evalListWithEnv env image moves =
    let
        helper currentImage remainingMoves =
            case remainingMoves of
                [] ->
                    { lastImage = currentImage, error = Nothing }

                m :: newRemainingMoves ->
                    let
                        result =
                            evalWithEnv env currentImage m
                    in
                    case result.error of
                        Nothing ->
                            helper result.lastImage newRemainingMoves

                        Just _ ->
                            result
    in
    helper image moves


eval : Image -> List Move -> EvalResult
eval image moves =
    evalListWithEnv [] image moves
