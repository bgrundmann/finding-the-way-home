module EvalResult exposing
    ( BacktraceStep(..)
    , EvalError
    , EvalResult
    , Problem(..)
    , addBacktrace
    , reportError
    , viewError
    )

import Element exposing (Element, column, fill, paragraph, row, spacing, text, width)
import Element.Font as Font
import ElmUiUtils exposing (boldMono, mono)
import Image exposing (Image, PileName)
import Move exposing (ExprValue(..), MoveDefinition)
import Palette


type Problem
    = NotEnoughCards { expected : Int, inPile : PileName, got : Int }
    | NoSuchPile { name : PileName }
    | TemporaryPileNotEmpty { names : List PileName, moveDefinition : MoveDefinition }
    | Bug String
    | EarlyExit


{-| First element in the list is the first move that happened.
Last element is the move that failed.
-}
type alias Backtrace =
    List
        { location : Int -- Nth move in the current list of moves
        , step : BacktraceStep
        }


type BacktraceStep
    = BtRepeat { nth : Int, total : Int }
    | BtDo MoveDefinition (List Move.Expr) (List ExprValue)


type alias EvalError =
    { problem : Problem
    , backtrace : Backtrace
    }


{-| This records the result of evaluation. That is the last successfully computed
image. The number of steps in total taken. And if there was an error.
-}
type alias EvalResult =
    { lastImage : Image
    , error : Maybe EvalError
    , steps : Int
    }


reportError : Image -> Int -> Problem -> EvalResult
reportError image steps problem =
    { lastImage = image
    , error = Just { problem = problem, backtrace = [] }
    , steps = steps
    }


{-| If we are in an error case, add the backtrace info.
-}
addBacktrace : Int -> BacktraceStep -> EvalResult -> EvalResult
addBacktrace loc step result =
    case result.error of
        Nothing ->
            result

        Just e ->
            { result
                | error = Just { e | backtrace = { location = loc, step = step } :: e.backtrace }
            }


viewBacktrace : Backtrace -> Element msg
viewBacktrace backtrace =
    column [ width fill, spacing 5 ]
        (List.map
            (\{ location, step } ->
                paragraph [ spacing 5, width fill ]
                    (text "➥ "
                        :: mono (String.fromInt location ++ ": ")
                        :: (case step of
                                BtRepeat { nth, total } ->
                                    [ boldMono "repeat "
                                    , mono (String.fromInt nth)
                                    , text " of "
                                    , mono (String.fromInt total)
                                    ]

                                BtDo md exprs actuals ->
                                    mono md.name
                                        :: List.map2
                                            (\arg v ->
                                                case v of
                                                    Int i ->
                                                        row [] [ mono arg.name, mono "=", mono (String.fromInt i) ]

                                                    Pile p ->
                                                        row [] [ mono arg.name, mono "=", mono p ]
                                            )
                                            md.args
                                            actuals
                                        |> List.intersperse (text " ")
                           )
                    )
            )
            backtrace
        )


viewError : EvalError -> Element msg
viewError error =
    column [ width fill, spacing 20 ]
        [ viewProblem error.problem
        , viewBacktrace error.backtrace
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
