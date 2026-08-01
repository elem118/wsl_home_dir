-- making a webpage that shows my first github commit code

module Main exposing (..)

import Html exposing (..)
import Browser
import Http exposing (..)

main = 
        Browser.element
          { init = init
          , update = update
          , subscriptions = subscriptions
          , view = view
          }

type Model = 
          Failure
        | Loading
        | Success String

init : () -> (Model, Cmd Msg)
init _ =
        ( Loading
        , Http.get
            { -- url = "https://github.com/elem118/register/blob/main/README.md"
              -- url = "https://elm-lang.org/assets/public-opinion.txt" -- I think this is working and not the prev because this site is designed to display only text
              url = "https://raw.githubusercontent.com/elem118/register/refs/heads/main/README.md"
            , expect = Http.expectString GotText
            }
        )

type Msg =
        GotText (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
        case msg of
                GotText result ->
                        case result of
                                Ok fullText -> 
                                        (Success fullText, Cmd.none)
                                Err _ -> 
                                        (Failure, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = 
        Sub.none

view : Model -> Html Msg
view model =
        case model of
                Failure ->
                        text "Sorry pageload failed"
                Loading ->
                        text "...loading..."
                Success fullText ->
                        pre [] [text fullText]
