module Eval exposing (cardician, cardicianFromMoves)

import Cardician exposing (Cardician)
import List.Extra
import Move exposing (Expr(..), ExprValue(..), Move(..), UserDefinedOrPrimitive(..))
import Primitives


type alias Env =
    List (List ExprValue)


cardicianWithEnv : Env -> Move -> Cardician ()
cardicianWithEnv env move =
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
        Repeat expr moves ->
            case replaceArgumentByValue env expr of
                Int n ->
                    cardicianFromMovesWithEnv env moves
                        |> List.repeat n
                        |> List.foldl Cardician.compose (Cardician.return ())

                Pile _ ->
                    Cardician.fail "Internal error: type checker failed"

        Do { body } actuals ->
            let
                actualValues =
                    List.map (replaceArgumentByValue env) actuals
            in
            case body of
                Primitive p ->
                    Primitives.cardicianOfPrimitive p actualValues

                UserDefined { moves } ->
                    cardicianFromMovesWithEnv (actualValues :: env) moves


cardicianFromMovesWithEnv : Env -> List Move -> Cardician ()
cardicianFromMovesWithEnv env moves =
    List.map (cardicianWithEnv env) moves
        |> List.foldl Cardician.compose (Cardician.return ())


cardicianFromMoves : List Move -> Cardician ()
cardicianFromMoves moves =
    cardicianFromMovesWithEnv [] moves


cardician : Move -> Cardician ()
cardician move =
    cardicianWithEnv [] move



{-

   {-| Create a cardician who can perform the given moves.
   -}
   cardician : Move ExprValue -> Cardician ()
   cardician move =
       case move of
           Repeat nExpr moves ->
               case nExpr of
                   Int n ->
                       cardicianFromMoves moves
                           |> List.repeat n
                           |> List.foldl Cardician.compose (Cardician.return ())

                   Pile _ ->
                       Cardician.fail "Internal error: type checker failed"

           Do { body } actuals ->
               case body of
                   UserDefined { moves } ->
                       -- TODO: Move Move.substituteArguments here
                       case Move.substituteArguments identity actuals moves of
                           Err msg ->
                               Cardician.fail ("Internal error: substitution failed " ++ msg)

                           Ok substitutedMoves ->
                               cardicianFromMoves substitutedMoves

                   Primitive p ->
                       Primitives.cardicianOfPrimitive p actuals


   cardicianFromMoves : List (Move ExprValue) -> Cardician ()
   cardicianFromMoves moves =
       List.map cardician moves
           |> List.foldl Cardician.compose (Cardician.return ())

-}
