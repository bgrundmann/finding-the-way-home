module Eval exposing (eval)

import EvalResult
    exposing
        ( EvalResult
        , EvalTrace(..)
        , Problem(..)
        , reportError
        )
import Image exposing (Image)
import List.Extra
import Move
    exposing
        ( Expr(..)
        , ExprValue(..)
        , Move(..)
        , MoveDefinition
        , UserDefinedOrPrimitive(..)
        )
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
    , continue : EvalResult -> Bool
    }


type alias MakeTrace =
    EvalResult.MoveInList -> EvalTrace


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
                        result.trace
                        (TemporaryPileNotEmpty { names = List.map .nameInSource piles, moveDefinition = md })


{-| If evaluation succeded, increase the steps count.
-}
increaseSteps : Env -> EvalResult -> EvalResult
increaseSteps env result =
    case result.error of
        Just _ ->
            result

        Nothing ->
            let
                nextResult =
                    { result | steps = result.steps + 1 }
            in
            if env.continue nextResult then
                nextResult

            else
                reportError result.lastImage result.steps result.trace EarlyExit


evalWithEnv : Env -> Image -> EvalTrace -> Int -> Int -> Move -> EvalResult
evalWithEnv env image evalTrace steps location move =
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
                                { lastImage = currentImage
                                , steps = stepsAcc
                                , error = Nothing

                                -- TODO unclear if that is right
                                , trace = evalTrace
                                }

                            else
                                let
                                    repeatEvalTrace =
                                        EVRepeat { n = n, total = times, prev = evalTrace }

                                    result =
                                        evalListWithEnv env currentImage repeatEvalTrace stepsAcc moves
                                in
                                case result.error of
                                    Nothing ->
                                        helper (n + 1) result.lastImage result.steps

                                    Just _ ->
                                        result
                    in
                    helper 1 image steps

                Pile _ ->
                    reportError image steps evalTrace (Bug "Type checker failed")

        Do ({ body } as md) actuals ->
            -- Before the actual call we need to check if we are supposed to stop here
            let
                actualValues =
                    List.map (replaceArgumentByValue env) actuals

                beforeCallResult =
                    { lastImage = image
                    , steps = steps
                    , error = Nothing
                    , trace = evalTrace
                    }
                        |> increaseSteps env
            in
            case beforeCallResult.error of
                Just _ ->
                    beforeCallResult

                Nothing ->
                    case body of
                        Primitive p ->
                            -- Here we pass in the original steps again, because in total
                            -- we only want to increase once
                            Primitives.eval image steps evalTrace p actualValues
                                |> increaseSteps env

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

                                userDefinedEvalTrace =
                                    EVUserDefined { def = md, actuals = actualValues, prev = evalTrace }

                                result =
                                    evalListWithEnv
                                        { env
                                            | tempCounter = newTempCounter
                                            , scoped = newScoped
                                        }
                                        image
                                        userDefinedEvalTrace
                                        beforeCallResult.steps
                                        moves
                            in
                            result
                                |> checkTemporaryPilesAreGone actualTemporaryPiles md



--                              |> increaseSteps continue
-- Leaving a user Definition counts as one


evalListWithEnv : Env -> Image -> MakeTrace -> Int -> List Move -> EvalResult
evalListWithEnv env image makeTrace steps moves =
    let
        helper currentImage stepsAcc location remainingMoves =
            case remainingMoves of
                [] ->
                    { lastImage = currentImage
                    , steps = stepsAcc
                    , error = Nothing
                    , trace = makeTrace { moves = moves, n = location }
                    }

                m :: newRemainingMoves ->
                    let
                        result =
                            evalWithEnv env currentImage (makeTrace { moves = moves, n = location }) stepsAcc location m
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
    let
        env =
            { tempCounter = 0, scoped = [], continue = continue }

        trace =
            EVTop
    in
    evalListWithEnv env image trace 0 moves
