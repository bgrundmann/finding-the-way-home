module Move exposing
    ( Argument
    , ArgumentKind(..)
    , Expr(..)
    , ExprValue(..)
    , Move(..)
    , MoveDefinition
    , Primitive(..)
    , UserDefinedOrPrimitive(..)
    , backwardsMoves
    , repeatSignature
    , signature
    , substituteArguments
    )

import Card
import Cardician exposing (Cardician, andThen, fail)
import Dict
import Image exposing (PileName)
import List.Extra
import Pile exposing (Pile)
import Result.Extra


type alias Argument =
    { name : String
    , kind : ArgumentKind
    }


type alias MoveDefinition =
    { name : String
    , args : List Argument
    , doc : String
    , body : UserDefinedOrPrimitive
    }


type alias UserDefinedMove =
    { definitions : List MoveDefinition
    , moves : List (Move Expr)
    }


type UserDefinedOrPrimitive
    = UserDefined UserDefinedMove
    | Primitive Primitive


type Primitive
    = Cut
    | Turnover


type Move arg
    = Repeat arg (List (Move arg))
    | Do MoveDefinition (List arg)


type Expr
    = ExprArgument { name : String, ndx : Int, kind : ArgumentKind }
    | ExprValue ExprValue


type ExprValue
    = Pile PileName
    | Int Int


type ArgumentKind
    = KindInt
    | KindPile


repeatSignature : String
repeatSignature =
    "repeat N\n  move1\n  move2\n  ...\nend"


{-| The signature is a human readable representation of a definitions names and arguments.
-}
signature : MoveDefinition -> String
signature { name, args } =
    name ++ " " ++ String.join " " (args |> List.map .name)


{-| Replace Arguments by their values.
The passed in Array must match the the args Array of the definition
the list of moves are part of. Or be the empty array.
-}
substituteArguments : (ExprValue -> a) -> List a -> List (Move Expr) -> Result String (List (Move a))
substituteArguments packValue actuals moves =
    let
        substExpr expr =
            case expr of
                ExprArgument { name, ndx } ->
                    case List.Extra.getAt ndx actuals of
                        Nothing ->
                            Err ("Internal error -- couldn't get " ++ name ++ " at " ++ String.fromInt ndx)

                        Just value ->
                            Ok value

                ExprValue v ->
                    Ok (packValue v)

        substMove move =
            case move of
                Repeat arg rmoves ->
                    Result.map2 Repeat
                        (substExpr arg)
                        (substituteArguments packValue actuals rmoves)

                Do def exprs ->
                    Result.map (\values -> Do def values)
                        (List.map substExpr exprs |> Result.Extra.combine)
    in
    List.map substMove moves
        |> Result.Extra.combine


backwards : (ExprValue -> a) -> Move a -> Move a
backwards packValue move =
    case move of
        Repeat arg moves ->
            Repeat arg (backwardsMoves packValue moves)

        Do def exprs ->
            case ( def.body, exprs ) of
                ( Primitive Cut, [ n, from, to ] ) ->
                    Do def [ n, to, from ]

                ( Primitive Turnover, [ _ ] ) ->
                    move

                ( UserDefined { moves }, _ ) ->
                    let
                        movesWithArguments =
                            substituteArguments packValue exprs moves
                    in
                    case movesWithArguments of
                        Ok mvs ->
                            -- Using Repeat 1 moves to turn a list of moves into a single move
                            Repeat (packValue (Int 1)) (backwardsMoves packValue mvs)

                        Err _ ->
                            move

                ( _, _ ) ->
                    -- Can not happen because of the type checker
                    move


backwardsMoves : (ExprValue -> a) -> List (Move a) -> List (Move a)
backwardsMoves packValue moves =
    List.reverse (List.map (backwards packValue) moves)
