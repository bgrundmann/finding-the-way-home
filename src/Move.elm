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
    , moves : List Move
    }


type UserDefinedOrPrimitive
    = UserDefined UserDefinedMove
    | Primitive Primitive


type Primitive
    = Cut
    | Turnover


type Move
    = Repeat Expr (List Move)
    | Do MoveDefinition (List Expr)


type Expr
    = ExprArgument
        { name : String
        , ndx : Int -- 0 is first argument
        , up : Int -- 0 is the definition this is part of, 1 is 1 level up
        , kind : ArgumentKind
        }
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


backwards : Move -> Move
backwards move =
    case move of
        Repeat arg moves ->
            Repeat arg (backwardsMoves moves)

        Do def exprs ->
            case ( def.body, exprs ) of
                ( Primitive Cut, [ n, from, to ] ) ->
                    Do def [ n, to, from ]

                ( Primitive Turnover, [ _ ] ) ->
                    move

                ( UserDefined u, _ ) ->
                    Do { def | body = UserDefined { u | moves = backwardsMoves u.moves } } exprs

                ( _, _ ) ->
                    -- Can not happen because of the type checker
                    move


backwardsMoves : List Move -> List Move
backwardsMoves moves =
    List.reverse (List.map backwards moves)



{-
   {-| Replace Arguments by their values.
   The passed in Array must match the the args Array of the definition
   the list of moves are part of. Or be the empty array.
   -}
   substituteArguments : (ExprValue -> a) -> List a -> List (Move Expr) -> Result String (List (Move a))
   substituteArguments packValue actuals moves =
       let
           substExpr expr =
               case expr of
                   ExprArgument { name, ndx, up } ->
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
-}
