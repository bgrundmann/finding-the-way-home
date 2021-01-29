module Primitives exposing (cardicianOfPrimitive, primitiveCut, primitiveTurnover, primitives)

import Card
import Cardician exposing (Cardician, andThen, fail)
import Dict exposing (Dict)
import Image exposing (PileName)
import Move
    exposing
        ( Argument
        , ArgumentKind(..)
        , Expr(..)
        , ExprValue(..)
        , Move(..)
        , MoveDefinition
        , Primitive(..)
        , UserDefinedOrPrimitive(..)
        )
import Pile exposing (Pile)



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
    { name = name, args = args, body = Primitive p, doc = "" }


primitiveTurnover : MoveDefinition
primitiveTurnover =
    primitive "turnover" [ pileArg "pile" ] Turnover


primitiveCut : MoveDefinition
primitiveCut =
    primitive "cut" [ intArg "N", pileArg "from", pileArg "to" ] Cut


primitives : Dict String MoveDefinition
primitives =
    [ primitiveCut
    , primitiveTurnover
    ]
        |> List.map (\m -> ( m.name, m ))
        |> Dict.fromList
