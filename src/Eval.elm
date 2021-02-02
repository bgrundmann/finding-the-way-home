module Eval exposing (eval)

import EvalResult exposing (BacktraceStep(..), EvalResult, Problem(..), addBacktrace, reportError)
import Image exposing (Image)
import List.Extra
import Move exposing (Expr(..), ExprValue(..), Move(..), MoveDefinition, UserDefinedOrPrimitive(..))
import Primitives


{-| The Evaluation environment.
The names used by the temporary piles are of this form:
<increasing global integer>-<name-as-used-in-source>
-}
type alias Env =
    { scoped :
        List
            { actuals : List ExprValue
            , temporaryPiles : List { nameInSource : String, nameInPile : String }
            }
    , tempCounter : Int
    }


checkTemporaryPilesAreGone :
    List { nameInSource : String, nameInPile : String }
    -> MoveDefinition
    -> EvalResult
    -> EvalResult
checkTemporaryPilesAreGone temporaryPileNames md result =
    case result.error of
        Just _ ->
            -- We already have a different error, let's not confuse matters
            result

        Nothing ->
            let
                names =
                    Image.names result.lastImage

                -- Images tend to have very small number of piles
                -- so being quadratic is fine
                overlap =
                    List.filter (\n -> List.member n.nameInPile names) temporaryPileNames
            in
            case overlap of
                [] ->
                    result

                piles ->
                    reportError result.lastImage
                        (TemporaryPileNotEmpty { names = List.map .nameInSource piles, moveDefinition = md })


evalWithEnv : Env -> Image -> Move -> EvalResult
evalWithEnv env image move =
    let
        replaceArgumentByValue e expr =
            case expr of
                ExprValue v ->
                    v

                ExprTemporaryPile { up, ndx } ->
                    let
                        value =
                            List.Extra.getAt up e.scoped
                                |> Maybe.andThen
                                    (\{ temporaryPiles } ->
                                        List.Extra.getAt ndx temporaryPiles
                                    )
                                |> Maybe.map .nameInPile
                    in
                    case value of
                        Nothing ->
                            Pile "INTERNAL ERROR"

                        Just v ->
                            Pile v

                ExprArgument { up, ndx } ->
                    let
                        value =
                            List.Extra.getAt up e.scoped
                                |> Maybe.andThen
                                    (\{ actuals } ->
                                        List.Extra.getAt ndx actuals
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
                            if n > times then
                                { lastImage = currentImage, error = Nothing }

                            else
                                let
                                    result =
                                        evalListWithEnv env currentImage moves
                                in
                                case result.error of
                                    Nothing ->
                                        helper (n + 1) result.lastImage

                                    Just _ ->
                                        result
                                            |> addBacktrace loc (BtRepeat n)
                    in
                    helper 1 image

                Pile _ ->
                    reportError image (Bug "Type checker failed")

        Do loc ({ name, body } as md) actuals ->
            let
                actualValues =
                    List.map (replaceArgumentByValue env) actuals
            in
            case body of
                Primitive p ->
                    Primitives.eval image p actualValues
                        |> addBacktrace loc (BtDo md actualValues)

                UserDefined { moves, temporaryPiles } ->
                    let
                        actualTemporaryPiles =
                            List.indexedMap
                                (\ndx tempPile ->
                                    { nameInSource = tempPile
                                    , nameInPile = "temp " ++ tempPile ++ " " ++ String.fromInt (ndx + env.tempCounter)
                                    }
                                )
                                temporaryPiles

                        newTempCounter =
                            env.tempCounter + List.length temporaryPiles

                        newScoped =
                            { actuals = actualValues, temporaryPiles = actualTemporaryPiles } :: env.scoped

                        result =
                            evalListWithEnv
                                { tempCounter = newTempCounter
                                , scoped = newScoped
                                }
                                image
                                moves
                    in
                    result
                        |> checkTemporaryPilesAreGone actualTemporaryPiles md
                        |> addBacktrace loc (BtDo md actualValues)


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
    evalListWithEnv { tempCounter = 0, scoped = [] } image moves
