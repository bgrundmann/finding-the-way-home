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


{-| Removes the given move and all moves that depend on it. Returns the moves
in topological sort order, so they could be compiled in that order.

    Returns the empty list if the move is not in the library.

-}
remove : MoveIdentifier -> MoveLibrary -> ( List MoveDefinition, MoveLibrary )
remove ident library =
    case dependencies ident library of
        [] ->
            ( [], library )

        deps ->
            let
                removalOrder =
                    List.reverse deps

                removeOneLeaf id ( removedAcc, libraryAcc ) =
                    case get id libraryAcc of
                        Nothing ->
                            -- This can't actually happen because dependencies has
                            -- already filtered out anything that isn't in the library
                            -- But we have to make the compiler happy
                            ( removedAcc, libraryAcc )

                        Just md ->
                            let
                                idText =
                                    Move.identifierText id

                                name =
                                    md.name

                                newLibrary =
                                    { definitions = Dict.remove idText libraryAcc.definitions
                                    , sameName =
                                        Dict.update name
                                            (\sameNameDefs ->
                                                let
                                                    l =
                                                        List.filter (\i -> i /= ident)
                                                            (Maybe.withDefault [] sameNameDefs)
                                                in
                                                if List.isEmpty l then
                                                    Nothing

                                                else
                                                    Just l
                                            )
                                            libraryAcc.sameName

                                    -- We are dealing with any mentions in values part of the used
                                    -- usedBy dictionary separately below
                                    , usedBy = Dict.remove idText libraryAcc.usedBy
                                    }
                            in
                            ( md :: removedAcc, newLibrary )

                -- No need to reverse the list again, because we want the
                -- inverse of removalOrder anyway
                ( res, resLibrary ) =
                    List.foldl removeOneLeaf ( [], library ) removalOrder

                toRemove =
                    Set.fromList (List.map Move.identifierText deps)
            in
            ( res, { resLibrary | usedBy = Dict.map (\_ set -> Set.diff set toRemove) resLibrary.usedBy } )


{-| Return the dependencies that are in the library in topological order. Including the original element
-}
dependencies : MoveIdentifier -> MoveLibrary -> List MoveIdentifier
dependencies ident library =
    let
        helper id =
            let
                usedBy =
                    getUsedBy id library
            in
            usedBy ++ List.concatMap helper usedBy
    in
    helper ident



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
