module Eval exposing (cardician, cardicianFromMoves)

import Cardician exposing (Cardician)
import Move exposing (ExprValue(..), Move(..), UserDefinedOrPrimitive(..))
import Primitives


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
