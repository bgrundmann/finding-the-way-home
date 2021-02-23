module Move exposing
    ( Argument
    , ArgumentKind(..)
    , Expr(..)
    , ExprValue(..)
    , Move(..)
    , MoveDefinition
    , MoveIdentifier
    , Primitive(..)
    , UserDefinedOrPrimitive(..)
    , backwardsMoves
    , exprKind
    , identifier
    , identifierText
    , makeIdentifier
    , repeatSignature
    , signature
    , unsafeIdentifierFromText
    , usesByDefinition
    )

import Image exposing (PileName)
import Pile exposing (Pile)


type alias Argument =
    { name : String
    , kind : ArgumentKind
    }


{-| A Move definition has the following elements.

def <name> <pilearg>|<NUMBERARG>...
[doc Documentation string...]
<nested def1>
<nested def2>
...
<move1>
<move2>
...
end

Inside a definition one can not use pile names directly. Only

  - The arguments to this definition
  - If this is a nested definition, the arguments to the enclosing definitions
    unless they are shadowed
  - Temporary piles. Those are piles whose names start with temp.
    When a definition is executed, temporary pile Names are guaranteed to be
    unique to the execution. Furthermore it is a checked runtime error for
    the execution to finish but the temporary pile to still exist (e.g.
    contain more than 0 cards).

-}
type alias MoveDefinition =
    { name : String
    , args : List Argument
    , identifier : MoveIdentifier
    , doc : String
    , body : UserDefinedOrPrimitive
    , path : List String -- [] == toplevel definition
    }


type MoveIdentifier
    = MoveIdentifier String


type alias UserDefinedMove =
    { definitions : List MoveDefinition
    , temporaryPiles : List String
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
    | Note String


type Expr
    = ExprArgument
        { name : String
        , ndx : Int -- 0 is first argument
        , up : Int -- 0 is the definition this is part of, 1 is 1 level up
        , kind : ArgumentKind
        }
    | ExprTemporaryPile { name : String, ndx : Int, up : Int }
    | ExprValue ExprValue


type ExprValue
    = Pile PileName
    | Int Int


type ArgumentKind
    = KindInt
    | KindPile


exprKind : Expr -> ArgumentKind
exprKind e =
    case e of
        ExprArgument { kind } ->
            kind

        ExprTemporaryPile _ ->
            KindPile

        ExprValue (Pile _) ->
            KindPile

        ExprValue (Int _) ->
            KindInt


repeatSignature : String
repeatSignature =
    "repeat N\n  move1\n  move2\n  ...\nend"


{-| The signature is a human readable representation of a definitions names and arguments.
-}
signature : MoveDefinition -> String
signature { name, args } =
    name ++ " " ++ String.join " " (args |> List.map .name)


identifier : MoveDefinition -> MoveIdentifier
identifier md =
    md.identifier


identifierText : MoveIdentifier -> String
identifierText (MoveIdentifier id) =
    id


{-| For use in MoveLibrary
-}
unsafeIdentifierFromText : String -> MoveIdentifier
unsafeIdentifierFromText s =
    MoveIdentifier s


makeIdentifier : String -> List ArgumentKind -> MoveIdentifier
makeIdentifier name argKinds =
    MoveIdentifier
        (name
            ++ " "
            ++ String.join ""
                (List.map
                    (\a ->
                        case a of
                            KindInt ->
                                "i"

                            KindPile ->
                                "p"
                    )
                    argKinds
                )
        )


backwards : Move -> Move
backwards move =
    case move of
        Repeat arg moves ->
            Repeat arg (backwardsMoves moves)

        Note n ->
            Note n

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


{-| What Toplevel moves does this move use? Does not include
non toplevel definitions as they are local to this move
definition.
-}
usesByDefinition : MoveDefinition -> List MoveDefinition
usesByDefinition md =
    case md.body of
        Primitive _ ->
            []

        UserDefined u ->
            List.concatMap usesOfMove u.moves


usesOfMove : Move -> List MoveDefinition
usesOfMove move =
    case move of
        Do d _ ->
            if List.isEmpty d.path then
                [ d ]

            else
                []

        Note _ ->
            []

        Repeat _ moves ->
            List.concatMap usesOfMove moves
