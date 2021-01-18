module Main exposing (..)

import Browser
import Card exposing (Card, Pile, Suit, Value, poker_deck)
import Cardician exposing (..)
import Dict exposing (Dict)
import Element exposing (Element, el, fill, height, padding, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Image exposing (Image)
import List
import Move exposing (Move(..))
import Result



-- MAIN


type alias PileName =
    String


cardician : Move -> Cardician ()
cardician move =
    case move of
        Deal { from, to } ->
            get from
                |> andThen
                    (\src ->
                        getOrEmpty to
                            |> andThen
                                (\dst ->
                                    case src of
                                        [] ->
                                            fail "Nothing left to deal"

                                        card :: rest ->
                                            put from rest
                                                |> andThen
                                                    (\() ->
                                                        put to (card :: dst)
                                                    )
                                )
                    )

        Cut _ ->
            fail "not supported"

        Assemble _ ->
            fail "not supported"

        Named _ _ ->
            fail "not supported"


apply : Move -> Image -> Result String Image
apply move image =
    let
        c =
            cardician move

        ( or_error, newImage ) =
            perform c image
    in
    case or_error of
        Err msg ->
            Err msg

        Ok () ->
            Ok newImage


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { initialImage : Image
    , movesText : String
    , finalImage : Image
    , moves : Result String (List Move)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialImage =
            [ ( "deck", poker_deck ) ]
    in
    ( { initialImage = initialImage, movesText = "", moves = Ok [], finalImage = initialImage }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Draw
    | SetMoves String


turnOver : Pile -> Pile
turnOver pile =
    List.reverse (List.map Card.turnOver pile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw ->
            ( model
            , Cmd.none
            )

        SetMoves text ->
            let
                moves =
                    Move.parseMoves text

                finalImage =
                    case moves of
                        Err _ ->
                            model.initialImage

                        Ok [] ->
                            Debug.log "a" model.initialImage

                        Ok (m :: ms) ->
                            case apply m model.initialImage of
                                Err _ ->
                                    Debug.log "b" model.initialImage

                                Ok i ->
                                    Debug.log "c" i
            in
            ( { model | movesText = text, moves = moves }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


blue =
    Element.rgb255 238 238 238


view : Model -> Html Msg
view model =
    let
        buttons =
            Element.row [ spacing 10 ]
                [ Input.button [ Element.padding 10, Border.rounded 5, Background.color blue ] { label = text "Draw", onPress = Just Draw }
                ]

        initialImageView =
            Image.view model.initialImage

        movesBorderColor =
            case model.moves of
                Err _ ->
                    Element.rgb255 255 0 0

                Ok _ ->
                    Element.rgb255 0 255 0

        movesView =
            Input.multiline [ width fill, height fill, Border.color movesBorderColor ]
                { label = Input.labelAbove [] (Element.text "Moves")
                , onChange = SetMoves
                , text = model.movesText
                , placeholder = Nothing
                , spellcheck = False
                }

        finalImageView =
            Image.view model.finalImage
    in
    Element.layout []
        (Element.column [ Element.padding 20, width fill, height fill, spacing 10 ]
            [ buttons
            , Element.row [ spacing 10, width fill, height fill ]
                [ initialImageView
                , movesView
                , finalImageView
                ]
            ]
        )
