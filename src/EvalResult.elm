module EvalResult exposing
    ( EvalError
    , EvalResult
    , EvalTrace(..)
    , MoveInList
    , Problem(..)
    , reportError
    , traceDepth
    , viewError
    , viewEvalTrace
    )

import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , paddingEach
        , paragraph
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import ElmUiUtils exposing (boldMono, mono)
import Image exposing (Image, PileName)
import Move exposing (ExprValue(..), Move, MoveDefinition)
import Palette exposing (largeTextSpacing, normalTextSpacing)
import ViewMove


type Problem
    = NotEnoughCards { expected : Int, inPile : PileName, got : Int }
    | NoSuchPile { name : PileName }
    | TemporaryPileNotEmpty { names : List PileName, moveDefinition : MoveDefinition }
    | Bug String
    | EarlyExit


{-| n meaning we are in the execution just before executing the corresponding move.
That is we have executed n of the moves in the list so far. That also means
that it n == List.length moves can happen.
-}
type alias MoveInList =
    { moves : List Move
    , n : Int
    }


type EvalTrace
    = EVTop MoveInList
    | EVRepeat { n : Int, total : Int, prev : EvalTrace } MoveInList
    | EVUserDefined { def : MoveDefinition, actuals : List ExprValue, prev : EvalTrace } MoveInList


traceReplaceMoveInList : MoveInList -> EvalTrace -> EvalTrace
traceReplaceMoveInList ml trace =
    case trace of
        EVTop _ ->
            EVTop ml

        EVRepeat rep _ ->
            EVRepeat rep ml

        EVUserDefined ud _ ->
            EVUserDefined ud ml


traceDepth : EvalTrace -> Int
traceDepth evalTrace =
    let
        helper res et =
            case et of
                EVTop _ ->
                    res

                EVRepeat { prev } _ ->
                    helper (res + 1) prev

                EVUserDefined { prev } _ ->
                    helper (res + 1) prev
    in
    helper 0 evalTrace


type alias EvalError =
    { problem : Problem
    }


{-| This records the result of evaluation. That is the last successfully computed
image. The number of steps in total taken. And if there was an error.
-}
type alias EvalResult =
    { lastImage : Image
    , error : Maybe EvalError
    , trace : EvalTrace
    , steps : Int
    }


reportError : Image -> Int -> EvalTrace -> Problem -> EvalResult
reportError image steps trace problem =
    { lastImage = image
    , error = Just { problem = problem }
    , trace = trace
    , steps = steps
    }


viewEvalTrace : ViewMove.ViewConfig -> Element.Color -> (EvalTrace -> msg) -> EvalTrace -> Element msg
viewEvalTrace viewConfig highlightColor gotoMsg trace =
    let
        stepped a b =
            column [ width fill, normalTextSpacing ]
                [ el [ width fill, Element.alignLeft ] a
                , el [ paddingEach { left = 20, right = 0, top = 0, bottom = 0 }, width fill ] b
                ]

        helper t showBelow =
            case t of
                EVTop _ ->
                    showBelow

                EVRepeat { n, total, prev } _ ->
                    helper prev
                        (stepped
                            (paragraph [ spacing 5 ]
                                [ boldMono "repeat "
                                , mono (String.fromInt n ++ " of " ++ String.fromInt total)
                                ]
                            )
                            showBelow
                        )

                EVUserDefined { def, actuals, prev } _ ->
                    helper prev
                        (stepped
                            (paragraph [ spacing 5 ]
                                ((mono def.name
                                    :: List.map2
                                        (\arg actual ->
                                            row [ width fill ]
                                                [ mono arg.name
                                                , mono "="
                                                , case actual of
                                                    Int i ->
                                                        mono (String.fromInt i)

                                                    Pile p ->
                                                        mono p
                                                ]
                                        )
                                        def.args
                                        actuals
                                 )
                                    |> List.intersperse (text " ")
                                )
                            )
                            showBelow
                        )

        moveInList =
            case trace of
                EVTop l ->
                    l

                EVRepeat _ l ->
                    l

                EVUserDefined _ l ->
                    l
    in
    helper trace <|
        column [ width fill, normalTextSpacing ]
            (List.indexedMap
                (\i move ->
                    let
                        highlighter =
                            if i == moveInList.n then
                                el
                                    [ width (px 10)
                                    , height (px 10)

                                    --, centerY
                                    , Background.color highlightColor
                                    , Border.rounded 5
                                    ]
                                    Element.none

                            else
                                Input.button
                                    [ width (px 10)
                                    , height (px 10)
                                    , Border.color Palette.grey
                                    , Border.width 1
                                    , Border.rounded 5
                                    , Element.pointer
                                    , Element.mouseOver [ Border.color Palette.greenBook ]
                                    ]
                                    { label = Element.none
                                    , onPress =
                                        gotoMsg
                                            (traceReplaceMoveInList
                                                { moves = moveInList.moves
                                                , n = i
                                                }
                                                trace
                                            )
                                            |> Just
                                    }
                    in
                    row [ width fill, spacing 5 ]
                        [ highlighter, ViewMove.view viewConfig move ]
                )
                moveInList.moves
            )


viewError : EvalError -> Element msg
viewError error =
    column [ width fill, spacing 20 ]
        [ viewProblem error.problem
        ]


viewProblem : Problem -> Element msg
viewProblem problem =
    case problem of
        EarlyExit ->
            Element.none

        Bug msg ->
            paragraph [ spacing 5, width fill, Font.bold, Font.color Palette.redBook ]
                [ text "INTERNAL ERROR CONTACT BENE ", text msg ]

        NotEnoughCards { expected, inPile, got } ->
            paragraph [ spacing 5, width fill ]
                [ text "Expected "
                , mono (String.fromInt expected)
                , text " cards in "
                , mono inPile
                , text " but got only "
                , mono (String.fromInt got)
                , text "!"
                ]

        NoSuchPile { name } ->
            paragraph [ spacing 5, width fill ]
                [ text "There is no pile called ", mono name, text "!" ]

        TemporaryPileNotEmpty { names, moveDefinition } ->
            case names of
                [ name ] ->
                    paragraph [ spacing 5, width fill ]
                        [ text "The temporary pile "
                        , mono name
                        , text " is not empty at the end of the call to "
                        , mono moveDefinition.name
                        , text "!"
                        ]

                _ ->
                    paragraph [ spacing 5, width fill ]
                        (text "The temporary piles "
                            :: (List.map mono names
                                    |> List.intersperse (text ", ")
                               )
                            ++ [ text " are not empty!" ]
                        )
