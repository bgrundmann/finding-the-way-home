module Move exposing
    ( Argument
    , ArgumentKind(..)
    , Expr(..)
    , ExprValue(..)
    , Location
    , Move(..)
    , MoveDefinition
    , Primitive(..)
    , UserDefinedOrPrimitive(..)
    , backwardsMoves
    , repeatSignature
    , signature
    )

import Image exposing (PileName)
import Pile exposing (Pile)


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


type alias Location =
    { row : Int }


type UserDefinedOrPrimitive
    = UserDefined UserDefinedMove
    | Primitive Primitive


type Primitive
    = Cut
    | Turnover


type Move
    = Repeat Location Expr (List Move)
    | Do Location MoveDefinition (List Expr)


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
        Repeat loc arg moves ->
            Repeat loc arg (backwardsMoves moves)

        Do loc def exprs ->
            case ( def.body, exprs ) of
                ( Primitive Cut, [ n, from, to ] ) ->
                    Do loc def [ n, to, from ]

                ( Primitive Turnover, [ _ ] ) ->
                    move

                ( UserDefined u, _ ) ->
                    Do loc { def | body = UserDefined { u | moves = backwardsMoves u.moves } } exprs

                ( _, _ ) ->
                    -- Can not happen because of the type checker
                    move


backwardsMoves : List Move -> List Move
backwardsMoves moves =
    List.reverse (List.map backwards moves)
