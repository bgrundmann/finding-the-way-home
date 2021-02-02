module EvalResult exposing (EvalError, EvalResult, Problem(..), addBacktrace, reportError, viewError)

import Element exposing (Element, column, fill, paragraph, row, spacing, text, width)
import Element.Font as Font
import ElmUiUtils exposing (mono)
import Image exposing (Image, PileName)
import Palette


type alias Location =
    { row : Int }


type Problem
    = NotEnoughCards { expected : Int, inPile : PileName, got : Int }
    | NoSuchPile { name : PileName }
    | TemporaryPileNotEmpty { names : List PileName }
    | Bug String


type alias EvalError =
    { problem : Problem
    , backtrace : List { location : Location }
    }


type alias EvalResult =
    { lastImage : Image
    , error : Maybe EvalError
    }


reportError : Image -> Problem -> EvalResult
reportError image problem =
    { lastImage = image
    , error = Just { problem = problem, backtrace = [] }
    }


{-| If we are in an error case, add the backtrace info.
-}
addBacktrace : Location -> EvalResult -> EvalResult
addBacktrace loc result =
    case result.error of
        Nothing ->
            result

        Just e ->
            { result
                | error = Just { e | backtrace = { location = loc } :: e.backtrace }
            }


viewError : EvalError -> Element msg
viewError error =
    case error.problem of
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

        TemporaryPileNotEmpty { names } ->
            case names of
                [ name ] ->
                    paragraph [ spacing 5, width fill ]
                        [ text "The temporary pile "
                        , mono name
                        , text " is not empty!"
                        ]

                _ ->
                    paragraph [ spacing 5, width fill ]
                        (text "The temporary piles "
                            :: (List.map mono names
                                    |> List.intersperse (text ", ")
                               )
                            ++ [ text " are not empty!" ]
                        )
