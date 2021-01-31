module EvalResult exposing (EvalError, EvalResult, reportError)

import Image exposing (Image)


type alias EvalError =
    { message : String
    }


type alias EvalResult =
    { lastImage : Image
    , error : Maybe EvalError
    }


reportError : Image -> String -> EvalResult
reportError image message =
    { lastImage = image, error = Just { message = message } }
