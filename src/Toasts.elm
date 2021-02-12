module Toasts exposing (Msg, Toast, Toasts, add, init, toast, update, view)

import Dict exposing (Dict)
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , maximum
        , padding
        , paragraph
        , px
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy
import Palette
import Process
import Task
import Time


type alias ToastID =
    Int


type Toasts
    = Toasts
        { toasts : Dict ToastID Toast
        , nextId : ToastID
        }


type alias Toast =
    { msg : String
    , bgColor : Element.Color
    , fontColor : Element.Color
    }


type Msg
    = Timeout ToastID


init : Toasts
init =
    Toasts { toasts = Dict.empty, nextId = 1 }


{-| A success toast.
-}
toast : String -> Toast
toast msg =
    { msg = msg, bgColor = Palette.greenBook, fontColor = Palette.white }


{-| Add a toast.
-}
add : Toast -> Toasts -> ( Toasts, Cmd Msg )
add t (Toasts model) =
    let
        id =
            model.nextId

        newNextId =
            id + 1

        timeoutTask =
            Process.sleep (5 * 1000)
    in
    ( Toasts { toasts = Dict.insert id t model.toasts, nextId = newNextId }
    , Task.perform (always (Timeout id)) timeoutTask
    )


update : Msg -> Toasts -> Toasts
update msg (Toasts model) =
    case msg of
        Timeout toastid ->
            Toasts { model | toasts = Dict.remove toastid model.toasts }


viewToast : Toast -> Element msg
viewToast { msg, bgColor, fontColor } =
    paragraph
        [ Background.color bgColor
        , Font.color fontColor
        , Border.rounded 10
        , padding 15
        , spacing 10
        , Border.width 1
        , Border.color Palette.black
        , Border.glow Palette.grey 2
        , width (maximum 250 fill)
        ]
        [ text msg ]


doView : Toasts -> Element msg
doView (Toasts model) =
    column
        [ spacing 10
        , Element.alignRight
        , Element.moveDown 20
        , Element.moveLeft 30
        ]
        (Dict.values model.toasts
            |> List.map viewToast
        )


{-| Use this in your layouts attribute list.
-}
view : Toasts -> Element.Attribute msg
view toasts =
    Element.inFront (Element.Lazy.lazy doView toasts)
