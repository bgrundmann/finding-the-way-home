module Move exposing
    ( Argument
    , ArgumentKind(..)
    , Expr(..)
    , ExprValue(..)
    , Move(..)
    , MoveDefinition
    , MovesOrPrimitive(..)
    , cardicianFromMoves
    , primitives
    , repeatSignature
    , signature
    , substituteArguments
    )

import Card exposing (Card, Pile)
import Cardician exposing (Cardician, andThen, fail)
import Dict exposing (Dict)
import Image exposing (PileName)
import List.Extra
import Result.Extra


type alias Argument =
    { name : String, kind : ArgumentKind }


type alias MoveDefinition =
    { name : String
    , args : List Argument
    , movesOrPrimitive : MovesOrPrimitive
    }


type alias ArgDefinition =
    { name : String, kind : ArgumentKind }


type MovesOrPrimitive
    = Moves (List (Move Expr))
    | Primitive (List ExprValue -> Cardician ())


type Move arg
    = Repeat arg (List (Move arg))
    | Do MoveDefinition (List arg)


repeatSignature : String
repeatSignature =
    "repeat N\n  move1\n  move2\n  ...\nend"


type Expr
    = ExprArgument { name : String, ndx : Int, kind : ArgumentKind }
    | ExprValue ExprValue


type ExprValue
    = Pile PileName
    | Int Int


type ArgumentKind
    = KindInt
    | KindPile


type TypeError
    = ExpectedPileGotInt
    | ExpectedIntGotPile


{-| The signature is a human readable representation of a definitions names and arguments.
-}
signature : MoveDefinition -> String
signature { name, args } =
    name ++ " " ++ String.join " " (args |> List.map .name)


{-| Replace Arguments by their values.
The passed in Array must match the the args Array of the definition
the list of moves are part of. Or be the empty array.
-}
substituteArguments : List ExprValue -> List (Move Expr) -> Result String (List (Move ExprValue))
substituteArguments actuals moves =
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
                    Ok v

        substMove move =
            case move of
                Repeat arg rmoves ->
                    Result.map2 Repeat
                        (substExpr arg)
                        (substituteArguments actuals rmoves)

                Do def exprs ->
                    Result.map (\values -> Do def values)
                        (List.map substExpr exprs |> Result.Extra.combine)
    in
    List.map substMove moves
        |> Result.Extra.combine



--- Primitives


turnover : Pile -> Pile
turnover pile =
    List.reverse (List.map Card.turnOver pile)


bugInTypeCheckerOrPrimitiveDef : String -> Cardician ()
bugInTypeCheckerOrPrimitiveDef name =
    fail ("Bug in type checker or definition of " ++ name)


primitiveTurnover : List ExprValue -> Cardician ()
primitiveTurnover args =
    case args of
        [ Pile name ] ->
            Cardician.take name
                |> andThen
                    (\cards ->
                        Cardician.put name (turnover cards)
                    )

        _ ->
            bugInTypeCheckerOrPrimitiveDef "turnover"


primitiveCut : List ExprValue -> Cardician ()
primitiveCut args =
    case args of
        [ Int n, Pile from, Pile to ] ->
            Cardician.cutOff n from
                |> andThen (Cardician.put to)

        _ ->
            bugInTypeCheckerOrPrimitiveDef "cut"


primitives =
    let
        int name =
            { name = name, kind = KindInt }

        pile name =
            { name = name, kind = KindPile }

        prim name args p =
            { name = name, args = args, movesOrPrimitive = Primitive p }
    in
    [ prim "turnover" [ pile "pile" ] primitiveTurnover
    , prim "cut" [ int "N", pile "from", pile "to" ] primitiveCut
    ]


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

        Do { name, movesOrPrimitive, args } actuals ->
            case movesOrPrimitive of
                Moves moves ->
                    case substituteArguments actuals moves of
                        Err msg ->
                            Cardician.fail ("Internal error: substitution failed " ++ msg)

                        Ok substitutedMoves ->
                            cardicianFromMoves substitutedMoves

                Primitive p ->
                    p actuals


cardicianFromMoves : List (Move ExprValue) -> Cardician ()
cardicianFromMoves moves =
    List.map cardician moves
        |> List.foldl Cardician.compose (Cardician.return ())
