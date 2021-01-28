module Move exposing
    ( Argument
    , ArgumentKind(..)
    , Expr(..)
    , ExprValue(..)
    , Move(..)
    , MoveDefinition
    , MovesOrPrimitive(..)
    , backwardsMoves
    , cardicianFromMoves
    , primitives
    , repeatSignature
    , signature
    , substituteArguments
    )

import Card
import Cardician exposing (Cardician, andThen, fail)
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
    , movesOrPrimitive : MovesOrPrimitive
    }


type MovesOrPrimitive
    = Moves (List (Move Expr))
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
            case ( def.movesOrPrimitive, exprs ) of
                ( Primitive Cut, [ n, from, to ] ) ->
                    Do def [ n, to, from ]

                ( Primitive Turnover, [ _ ] ) ->
                    move

                ( Moves moves, _ ) ->
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



--- Primitives


turnover : Pile -> Pile
turnover pile =
    List.reverse (List.map Card.turnover pile)


bugInTypeCheckerOrPrimitiveDef : Primitive -> Cardician ()
bugInTypeCheckerOrPrimitiveDef p =
    let
        name =
            case p of
                Cut ->
                    "cut"

                Turnover ->
                    "turnover"
    in
    fail ("Bug in type checker or definition of " ++ name)


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


cardicianOfPrimitive : Primitive -> List ExprValue -> Cardician ()
cardicianOfPrimitive =
    decodeActuals
        { turnover =
            \name ->
                Cardician.take name
                    |> andThen
                        (\cards ->
                            Cardician.put name (turnover cards)
                        )
        , cut =
            \n from to ->
                Cardician.cutOff n from
                    |> andThen (Cardician.put to)
        , decodingError = bugInTypeCheckerOrPrimitiveDef
        }


intArg : String -> Argument
intArg name =
    { name = name, kind = KindInt }


pileArg : String -> Argument
pileArg name =
    { name = name, kind = KindPile }


primitive : String -> List Argument -> Primitive -> MoveDefinition
primitive name args p =
    { name = name, args = args, movesOrPrimitive = Primitive p, doc = "" }


primitiveTurnover : MoveDefinition
primitiveTurnover =
    primitive "turnover" [ pileArg "pile" ] Turnover


primitiveCut : MoveDefinition
primitiveCut =
    primitive "cut" [ intArg "N", pileArg "from", pileArg "to" ] Cut


primitives : List MoveDefinition
primitives =
    [ primitiveCut
    , primitiveTurnover
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
                    case substituteArguments identity actuals moves of
                        Err msg ->
                            Cardician.fail ("Internal error: substitution failed " ++ msg)

                        Ok substitutedMoves ->
                            cardicianFromMoves substitutedMoves

                Primitive p ->
                    cardicianOfPrimitive p actuals


cardicianFromMoves : List (Move ExprValue) -> Cardician ()
cardicianFromMoves moves =
    List.map cardician moves
        |> List.foldl Cardician.compose (Cardician.return ())
