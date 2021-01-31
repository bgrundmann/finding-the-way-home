module EvalResult exposing (EvalError, EvalResult, addBacktrace, reportError)

import Image exposing (Image)


type alias Location =
    { row : Int }


type alias EvalError =
    { message : String
    , backtrace : List { location : Location }
    }


type alias EvalResult =
    { lastImage : Image
    , error : Maybe EvalError
    }


reportError : Image -> String -> EvalResult
reportError image message =
    { lastImage = image
    , error = Just { message = message, backtrace = [] }
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
