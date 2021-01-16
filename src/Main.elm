module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  { card : Card
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model { face = Face (Ace, Spades), back = Back Red }
  , Cmd.none
  )


type Suit
  = Clubs
  | Spades
  | Hearts
  | Diamonds


type Value
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King


type alias RegularFace = (Value, Suit)

type BackColor = Red | Green | Blue | Blank

type CardDesign
  = Face RegularFace
  | Back BackColor


type alias Card =
  { face : CardDesign
  , back : CardDesign
  }


-- UPDATE


type Msg
  = Draw



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , Cmd.none
      )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Draw ] [ text "Draw" ]
    , div [ style "font-size" "12em" ] [ text (viewCard model.card) ]
    ]

viewCard : Card -> String
viewCard { face, back } =
  viewCardDesign face

viewCardDesign : CardDesign -> String
viewCardDesign d =
  case d of
    Face rf -> viewRegularFace rf
    Back b  -> viewBack b


viewBack : BackColor -> String
viewBack b = ""
  

viewRegularFace : RegularFace -> String
viewRegularFace (value, suit) =
  case value of
    Ace -> "🂡"
    Two -> "🂢"
    Three -> "🂣"
    Four -> "🂤"
    Five -> "🂥"
    Six -> "🂦"
    Seven -> "🂧"
    Eight -> "🂨"
    Nine -> "🂩"
    Ten -> "🂪"
    Jack -> "🂫"
    Queen -> "🂭"
    King -> "🂮"


