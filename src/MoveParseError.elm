module MoveParseError exposing (Context, DeadEnd, Expectation(..), MoveParseError, Problem(..), view)

import Dict
import Dict.Extra
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , paragraph
        , px
        , row
        , spacing
        , text
        , textColumn
        , width
        )
import Element.Border as Border
import Element.Font as Font
import ElmUiUtils exposing (mono)
import List.Extra
import Move exposing (ArgumentKind(..), Expr, MoveDefinition)
import Palette exposing (largeTextSpacing, normalTextSpacing)
import Parser.Advanced
import ViewMove


type alias Context =
    ()


type alias MoveParseError =
    List DeadEnd


type alias DeadEnd =
    Parser.Advanced.DeadEnd Context Problem


type Problem
    = UnknownMove { name : String, options : List MoveDefinition }
    | NoSuchArgument { name : String, kind : ArgumentKind }
    | Expected Expectation
    | InvalidMoveInvocation { options : List MoveDefinition, actuals : List Expr }
    | DuplicateDefinition MoveDefinition


type Expectation
    = EPileName
    | ENumberName
    | EInt
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
                    row [] [ text "a name of numeric argument (e.g. ", mono "N", text ")" ]

                EInt ->
                    row [] [ text "a number (e.g. ", mono "52", text ")" ]

                EEndOfLine ->
                    text "the next line"

                EEndOfInput ->
                    text "the end"

                EMoveName ->
                    row [] [ text "a move name (e.g. ", mono "deal", text ")" ]

        viewProblem problem =
            case problem of
                UnknownMove { name, options } ->
                    column [ largeTextSpacing, width fill ]
                        (paragraph [ normalTextSpacing ]
                            [ el [ Font.bold ] (text "Don't know how to do: "), mono name ]
                            :: List.map
                                (\md ->
                                    textColumn [ normalTextSpacing, width fill ]
                                        [ paragraph [ width fill, normalTextSpacing ] [ mono (Move.signature md) ]
                                        , paragraph [ width fill, normalTextSpacing ] [ text md.doc ]
                                        ]
                                )
                                options
                        )

                Expected ex ->
                    paragraph [] [ text "Expected ", viewExpectation ex ]

                NoSuchArgument { name, kind } ->
                    case kind of
                        KindInt ->
                            paragraph [ normalTextSpacing, width fill ]
                                [ mono name
                                , text " looks like a number argument, but no such argument was defined"
                                ]

                        KindPile ->
                            textColumn [ largeTextSpacing, width fill ]
                                [ paragraph [ normalTextSpacing, width fill ]
                                    [ text "There is neither an argument nor a temporary pile called "
                                    , mono name
                                    , text "."
                                    ]
                                , paragraph [ normalTextSpacing, width fill ]
                                    [ text
                                        """Note that inside a definition you cannot refer to a pile directly.
                                   Everything must either be an argument to the definition or be
                                   explicitly defined as a temporary."""
                                    ]
                                ]

                InvalidMoveInvocation { options, actuals } ->
                    column [ largeTextSpacing, width fill ]
                        (List.map
                            (\md ->
                                textColumn [ normalTextSpacing, width fill ]
                                    [ paragraph [ width fill, normalTextSpacing ] [ mono <| Move.signature md ]
                                    , paragraph [ width fill, normalTextSpacing ] [ text md.doc ]
                                    ]
                            )
                            options
                        )

                DuplicateDefinition previousDefinition ->
                    column [ largeTextSpacing, width fill ]
                        [ paragraph [ normalTextSpacing, width fill ]
                            [ text "There is already a previous definition with the same arguments." ]
                        , ViewMove.viewDefinition ViewMove.defaultConfig previousDefinition
                        ]

        relevantLineAndPlace row col =
            case List.Extra.getAt (row - 1) (String.lines source) of
                Nothing ->
                    text "THIS SHOULD NOT HAPPEN"

                Just line ->
                    let
                        left =
                            String.left col line

                        right =
                            String.dropLeft col line
                                |> String.padRight 1 '_'
                    in
                    paragraph [ normalTextSpacing, Font.family [ Font.monospace ] ]
                        [ text left
                        , el
                            [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                            , Border.color Palette.redBook
                            ]
                            (text right)
                        ]

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
                            column [ normalTextSpacing ]
                                [ text "Expected one of: "
                                , row []
                                    [ el [ width (px 20) ] Element.none
                                    , exs
                                        |> List.map viewExpectation
                                        |> column [ normalTextSpacing ]
                                    ]
                                ]

                viewOtherProblems =
                    case otherProblems of
                        [] ->
                            Element.none

                        others ->
                            column [ normalTextSpacing ] (List.map viewProblem others)
            in
            column [ largeTextSpacing ]
                [ relevantLineAndPlace deadEnd.row deadEnd.col
                , viewExpectedProblems
                , viewOtherProblems
                ]
    in
    case gatherDeadEndsByLocation deadEnds of
        [] ->
            Element.none

        des ->
            column [ largeTextSpacing ] (List.map viewDeadEnd des)
