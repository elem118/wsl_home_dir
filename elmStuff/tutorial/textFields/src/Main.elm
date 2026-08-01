module Main exposing (..)

import Html exposing (..)
import Browser
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main =
        Browser.sandbox { init = init, update = update, view = view}

type alias Model = 
        { 
                input : String,
                output : String
        }

init : Model
init = { input = ""
       , output = ""}

type Msg = Change String | Reverse String

update msg model =
        case msg of
                Change str -> { model | input = str }
                Reverse str -> { model | output = String.reverse str }

view : Model -> Html Msg
view model = 
        div []
        [ input [ type_ "text", placeholder "Enter text here", value model.input , onInput Change] [ ]
        , div [] [ text model.output ]
        , button [ onClick (Reverse model.input) ] [ text "Reverse" ]
        ]
