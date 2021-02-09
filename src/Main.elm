module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Dict
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , fillPortion
        , height
        , minimum
        , mouseOver
        , padding
        , paddingXY
        , row
        , scale
        , scrollbarY
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Eval
import EvalResult exposing (EvalResult)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html)
import Image exposing (Image)
import ImageEditor
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Move exposing (ExprValue(..), Move(..), UserDefinedOrPrimitive(..))
import MoveEditor
import MoveParseError exposing (MoveParseError)
import MoveParser exposing (Definitions)
import Palette exposing (greenBook, redBook, white)
import Pile
import Ports
import Primitives exposing (primitives)
import Task



-- MODEL
-- In backwards mode we display the initial image on the right and evaluate the moves backwards


type alias Model =
    MoveEditor.Model


type alias Msg =
    MoveEditor.Msg


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Encode.Value -> ( Model, Cmd Msg )
init previousState =
    MoveEditor.init previousState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    MoveEditor.update msg model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


topBar : Element Msg
topBar =
    row [ spacing 10, padding 10, Background.color greenBook, Font.color white, width fill ]
        [ el [ Font.bold ] (text "🍺 Virtual Denis Behr")

        -- , Input.button [ mouseOver [ scale 1.1 ] ] { label = text "Save", onPress = Just Save }
        -- , Input.button [ mouseOver [ scale 1.1 ] ] { label = text "Load", onPress = Just SelectLoad }
        ]


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill ] <|
        column [ width fill, height fill ]
            [ topBar
            , MoveEditor.view model
            ]
