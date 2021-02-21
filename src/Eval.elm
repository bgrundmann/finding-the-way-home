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
            , temporaryPiles :
                List
                    { nameInSource : String
                    , nameInPile : String
                    }
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
                        result.steps
                        (TemporaryPileNotEmpty { names = List.map .nameInSource piles, moveDefinition = md })


{-| If evaluation succeded, increase the steps count.
-}
increaseSteps : (EvalResult -> Bool) -> EvalResult -> EvalResult
increaseSteps continue result =
    case result.error of
        Just _ ->
            result

        Nothing ->
            let
                nextResult =
                    { result | steps = result.steps + 1 }
            in
            if continue nextResult then
                nextResult

            else
                reportError result.lastImage result.steps EarlyExit


evalWithEnv : (EvalResult -> Bool) -> Env -> Image -> Int -> Int -> Move -> EvalResult
evalWithEnv continue env image steps location move =
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
        Repeat expr moves ->
            case replaceArgumentByValue env expr of
                Int times ->
                    let
                        helper n currentImage stepsAcc =
                            if n > times then
                                { lastImage = currentImage, steps = stepsAcc, error = Nothing }

                            else
                                let
                                    result =
                                        evalListWithEnv continue env currentImage stepsAcc moves
                                in
                                case result.error of
                                    Nothing ->
                                        helper (n + 1) result.lastImage result.steps

                                    Just _ ->
                                        result
                                            |> addBacktrace location (BtRepeat { nth = n, total = times })
                    in
                    helper 1 image steps

                Pile _ ->
                    reportError image steps (Bug "Type checker failed")

        Do ({ body } as md) actuals ->
            -- Before the actual call we need to check if we are supposed to stop here
            let
                actualValues =
                    List.map (replaceArgumentByValue env) actuals

                beforeCallResult =
                    { lastImage = image, steps = steps, error = Nothing }
                        |> increaseSteps continue
            in
            case beforeCallResult.error of
                Just _ ->
                    beforeCallResult
                        |> addBacktrace location (BtDo md actuals actualValues)

                Nothing ->
                    case body of
                        Primitive p ->
                            -- Here we pass in the original steps again, because in total
                            -- we only want to increase once
                            Primitives.eval image steps p actualValues
                                |> addBacktrace location (BtDo md actuals actualValues)
                                |> increaseSteps continue

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
                                        continue
                                        { tempCounter = newTempCounter
                                        , scoped = newScoped
                                        }
                                        image
                                        beforeCallResult.steps
                                        moves
                            in
                            result
                                |> checkTemporaryPilesAreGone actualTemporaryPiles md
                                |> addBacktrace location (BtDo md actuals actualValues)



--                              |> increaseSteps continue
-- Leaving a user Definition counts as one


evalListWithEnv : (EvalResult -> Bool) -> Env -> Image -> Int -> List Move -> EvalResult
evalListWithEnv continue env image steps moves =
    let
        helper currentImage stepsAcc location remainingMoves =
            case remainingMoves of
                [] ->
                    { lastImage = currentImage, steps = stepsAcc, error = Nothing }

                m :: newRemainingMoves ->
                    let
                        result =
                            evalWithEnv continue env currentImage stepsAcc location m
                    in
                    case result.error of
                        Nothing ->
                            helper result.lastImage result.steps (location + 1) newRemainingMoves

                        Just _ ->
                            result
    in
    helper image steps 0 moves


eval : (EvalResult -> Bool) -> Image -> List Move -> EvalResult
eval continue image moves =
    evalListWithEnv continue { tempCounter = 0, scoped = [] } image 0 moves
