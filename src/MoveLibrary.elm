module MoveLibrary exposing
    ( MoveLibrary
    , fromList
    , get
    , getByName
    , getUsedBy
    ,  insert
       --, remove

    , toList
    )

import Dict exposing (Dict)
import List.Extra
import Move exposing (ArgumentKind, MoveDefinition, MoveIdentifier, usesByDefinition)
import Set exposing (Set)


type alias MoveIdentifierText =
    String


type alias MoveLibrary =
    { definitions : Dict MoveIdentifierText MoveDefinition
    , sameName : Dict MoveIdentifierText (List MoveIdentifier)
    , usedBy : Dict MoveIdentifierText (Set MoveIdentifierText)
    }


toList : MoveLibrary -> List MoveDefinition
toList l =
    Dict.values l.definitions


empty : MoveLibrary
empty =
    { definitions = Dict.empty
    , sameName = Dict.empty
    , usedBy = Dict.empty
    }


fromList : List MoveDefinition -> MoveLibrary
fromList dl =
    List.foldl insert empty dl


{-| Insert the given definition into the library. Replaces any existing definition
with the same Identifier.
-}
insert : MoveDefinition -> MoveLibrary -> MoveLibrary
insert md library =
    let
        name =
            md.name

        identifier =
            Move.identifier md

        uses =
            usesByDefinition md
    in
    { definitions = Dict.insert (Move.identifierText identifier) md library.definitions
    , sameName =
        Dict.update
            name
            (\maybeSameNameMds ->
                identifier
                    :: List.filter (\id -> id /= identifier) (Maybe.withDefault [] maybeSameNameMds)
                    |> Just
            )
            library.sameName

    -- Todo: Remove previous uses if any
    , usedBy =
        List.foldl
            (\use usedByAcc ->
                Dict.update
                    (Move.identifier use |> Move.identifierText)
                    (\otherUses ->
                        Set.insert
                            (Move.identifierText identifier)
                            (Maybe.withDefault Set.empty otherUses)
                            |> Just
                    )
                    usedByAcc
            )
            library.usedBy
            uses
    }



{-
   remove : MoveIdentifier -> MoveLibrary -> MoveLibrary
   remove ident library =
       let
           ( name, argKinds ) =
               ident
       in
       { definitions =
           Dict.update
               name
               (Maybe.map
                   (\mdsWithSameName ->
                       List.filter (\( aks, _ ) -> aks /= argKinds) mdsWithSameName
                   )
               )
               library.definitions
       }
-}


get : MoveIdentifier -> MoveLibrary -> Maybe MoveDefinition
get ident library =
    Dict.get (Move.identifierText ident) library.definitions


getByName : String -> MoveLibrary -> List MoveDefinition
getByName name library =
    Dict.get name library.sameName
        |> Maybe.withDefault []
        |> List.filterMap (\i -> get i library)


getUsedBy : MoveIdentifier -> MoveLibrary -> List MoveIdentifier
getUsedBy ident library =
    Dict.get (Move.identifierText ident) library.usedBy
        |> Maybe.map Set.toList
        |> Maybe.withDefault []
        |> List.map Move.unsafeIdentifierFromText
