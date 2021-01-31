module MoveParseError exposing (Context, DeadEnd, Expectation(..), Problem(..), deadEndsToString)

import Dict
import Dict.Extra
import List.Extra
import Move exposing (ArgumentKind(..), MoveDefinition)
import Parser.Advanced


type alias Context =
    ()


type alias DeadEnd =
    Parser.Advanced.DeadEnd Context Problem


type Problem
    = UnknownMove String
    | NoSuchArgument String
    | Expected Expectation
    | ExpectedForArgument
        { move : Maybe MoveDefinition -- Nothing => Repeat
        , argName : String
        , argKind : ArgumentKind
        , options : List String
        }
    | DuplicateDefinition String


type Expectation
    = EPileName
    | ENumberName
    | EEndOfInput
    | EKeyword String
    | EEndOfLine
    | EMoveName


gatherDeadEndsByLocation : List DeadEnd -> List { row : Int, col : Int, problems : List Problem }
gatherDeadEndsByLocation deadEnds =
    let
        -- Problem lists are always small, so this is fine
        -- even so it is quadratic.  Also note that this is
        -- only the list of problems at the same location
        dedupProblems problems =
            case problems of
                [] ->
                    []

                [ x ] ->
                    [ x ]

                x :: xs ->
                    if List.member x xs then
                        dedupProblems xs

                    else
                        x :: dedupProblems xs
    in
    Dict.Extra.groupBy (\d -> ( d.row, d.col )) deadEnds
        |> Dict.toList
        |> List.map
            (\( ( row, col ), ds ) ->
                { row = row, col = col, problems = List.map .problem ds |> dedupProblems }
            )


deadEndsToString : String -> List DeadEnd -> String
deadEndsToString text deadEnds =
    let
        expectationToString ex =
            case ex of
                EKeyword s ->
                    "'" ++ s ++ "'"

                EPileName ->
                    "a pile name (e.g. deck)"

                ENumberName ->
                    "a number name (e.g. N)"

                EEndOfLine ->
                    "the next line"

                EEndOfInput ->
                    "the end"

                EMoveName ->
                    "a move name (e.g. 'deal')"

        problemToString problem =
            case problem of
                UnknownMove n ->
                    "Don't know how to do '" ++ n ++ "'"

                DuplicateDefinition d ->
                    "You already know how to '" ++ d ++ "'"

                Expected ex ->
                    "Expected " ++ expectationToString ex

                NoSuchArgument name ->
                    name ++ " looks like an argument, but no such argument was defined"

                ExpectedForArgument { move, argName, argKind, options } ->
                    let
                        ( moveSignature, doc ) =
                            case move of
                                Nothing ->
                                    ( Move.repeatSignature, "Repeat the given moves N times" )

                                Just m ->
                                    ( Move.signature m, m.doc )
                    in
                    case argKind of
                        KindInt ->
                            moveSignature
                                ++ "\n"
                                ++ doc
                                ++ "\n\nI need a number for "
                                ++ argName
                                ++ "\n"
                                ++ "Type the number (e.g. 52)"
                                ++ (case options of
                                        [] ->
                                            ""

                                        [ x ] ->
                                            " or " ++ x

                                        l ->
                                            " or one of " ++ String.join ", " l
                                   )

                        KindPile ->
                            moveSignature
                                ++ "\n"
                                ++ doc
                                ++ "\n\nI need a pilename for "
                                ++ argName
                                ++ "\n"
                                ++ "These are the piles I know about: "
                                ++ String.join ", " options

        relevantLineAndPlace row col =
            case List.Extra.getAt (row - 1) (String.lines text) of
                Nothing ->
                    "THIS SHOULD NOT HAPPEN"

                Just line ->
                    line ++ "\n" ++ String.repeat (col - 1) " " ++ "^\n"

        deadEndToString { row, col, problems } =
            let
                ( wrappedExpectedProblems, otherProblems ) =
                    List.partition
                        (\p ->
                            case p of
                                Expected _ ->
                                    True

                                _ ->
                                    False
                        )
                        problems

                expectedProblems =
                    List.map
                        (\e ->
                            case e of
                                Expected x ->
                                    x

                                _ ->
                                    -- Can't happen
                                    EKeyword ""
                        )
                        wrappedExpectedProblems

                expectedProblemsString =
                    case List.reverse expectedProblems of
                        [] ->
                            ""

                        [ ex ] ->
                            "Expected " ++ expectationToString ex ++ "\n"

                        ex :: exs ->
                            "Expected one of " ++ String.join ", " (List.map expectationToString (List.reverse exs)) ++ " or " ++ expectationToString ex ++ "\n"

                otherProblemsString =
                    case otherProblems of
                        [] ->
                            ""

                        others ->
                            String.join "\n" (List.map problemToString others)
            in
            relevantLineAndPlace row col ++ expectedProblemsString ++ otherProblemsString
    in
    case gatherDeadEndsByLocation deadEnds of
        [] ->
            ""

        des ->
            String.join "\n" (List.map deadEndToString des)
