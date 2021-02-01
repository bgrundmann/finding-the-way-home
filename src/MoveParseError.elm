module MoveParseError exposing (Context, DeadEnd, Expectation(..), MoveParseError, Problem(..), view)

import Dict
import Dict.Extra
import Element exposing (Element, column, el, fill, height, paragraph, row, spacing, text, textColumn, width)
import Element.Font as Font
import List.Extra
import Move exposing (ArgumentKind(..), MoveDefinition)
import Parser.Advanced


type alias Context =
    ()


type alias MoveParseError =
    List DeadEnd


type alias DeadEnd =
    Parser.Advanced.DeadEnd Context Problem


type Problem
    = UnknownMove String
    | NoSuchArgument { name : String, kind : ArgumentKind }
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


mono s =
    el [ Font.family [ Font.monospace ] ] (text s)


view : String -> List DeadEnd -> Element msg
view source deadEnds =
    let
        viewExpectation ex =
            case ex of
                EKeyword s ->
                    mono s

                EPileName ->
                    row [] [ text "a pile name (e.g. ", mono "deck", text ")" ]

                ENumberName ->
                    row [] [ text "a number name (e.g. ", mono "N", text ")" ]

                EEndOfLine ->
                    text "the next line"

                EEndOfInput ->
                    text "the end"

                EMoveName ->
                    row [] [ text "a move name (e.g. ", mono "deal", text ")" ]

        viewProblem problem =
            case problem of
                UnknownMove n ->
                    row [] [ text "Don't know how to do ", mono n ]

                DuplicateDefinition d ->
                    row [] [ text "You already know how to ", mono d ]

                Expected ex ->
                    row [] [ text "Expected ", viewExpectation ex ]

                NoSuchArgument { name, kind } ->
                    case kind of
                        KindInt ->
                            paragraph [ spacing 5, width fill ]
                                [ mono name
                                , text " looks like a number argument, but no such argument was defined"
                                ]

                        KindPile ->
                            textColumn [ spacing 20, width fill ]
                                [ paragraph [ spacing 5, width fill ]
                                    [ text "There is neither an argument nor a temporary pile called "
                                    , mono name
                                    , text "."
                                    ]
                                , paragraph [ spacing 5, width fill ]
                                    [ text
                                        """Note that inside a definition you cannot refer to a pile directly.
                                   Everything must either be an argument to the definition or be
                                   explicitly defined as a temporary."""
                                    ]
                                ]

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
                            column [ spacing 20, width fill ]
                                [ column [ spacing 5, width fill ]
                                    [ mono moveSignature
                                    , paragraph [ width fill, spacing 5 ] [ text doc ]
                                    ]
                                , column [ spacing 5, width fill ]
                                    [ paragraph [ spacing 5, width fill ]
                                        [ text "I need a number for ", mono argName ]
                                    , paragraph [ spacing 5, width fill ]
                                        (text "Type the number (e.g. "
                                            :: mono "52"
                                            :: text ")"
                                            :: (case options of
                                                    [] ->
                                                        []

                                                    [ x ] ->
                                                        [ text " or ", mono x ]

                                                    l ->
                                                        text " or one of "
                                                            :: (l |> List.map mono |> List.intersperse (text ", "))
                                               )
                                        )
                                    ]
                                ]

                        KindPile ->
                            column [ spacing 20, width fill ]
                                [ column [ spacing 5, width fill ]
                                    [ mono moveSignature
                                    , paragraph [ width fill, spacing 5 ] [ text doc ]
                                    ]
                                , column [ spacing 5, width fill ]
                                    [ paragraph [ spacing 5, width fill ]
                                        [ text "I need a pilename for ", mono argName ]
                                    , paragraph [ spacing 5, width fill ]
                                        (case options of
                                            [] ->
                                                []

                                            piles ->
                                                text "These are the piles I know about: "
                                                    :: (piles
                                                            |> List.map mono
                                                            |> List.intersperse (text ", ")
                                                       )
                                        )
                                    ]
                                ]

        relevantLineAndPlace row col =
            case List.Extra.getAt (row - 1) (String.lines source) of
                Nothing ->
                    text "THIS SHOULD NOT HAPPEN"

                Just line ->
                    el [ Font.family [ Font.monospace ] ]
                        (column [ spacing 5 ]
                            [ text line
                            , text (String.repeat (col - 1) " " ++ "^")
                            ]
                        )

        viewDeadEnd deadEnd =
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
                        deadEnd.problems

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

                viewExpectedProblems =
                    case List.reverse expectedProblems of
                        [] ->
                            Element.none

                        [ ex ] ->
                            row [] [ text "Expected ", viewExpectation ex ]

                        exs ->
                            row []
                                [ text "Expected one of "
                                , paragraph [ spacing 5 ]
                                    (exs |> List.map viewExpectation |> List.intersperse (text ", "))
                                ]

                viewOtherProblems =
                    case otherProblems of
                        [] ->
                            Element.none

                        others ->
                            column [ spacing 5 ] (List.map viewProblem others)
            in
            column [ spacing 5 ]
                [ relevantLineAndPlace deadEnd.row deadEnd.col
                , viewExpectedProblems
                , viewOtherProblems
                ]
    in
    case gatherDeadEndsByLocation deadEnds of
        [] ->
            Element.none

        des ->
            column [ spacing 10 ] (List.map viewDeadEnd des)
