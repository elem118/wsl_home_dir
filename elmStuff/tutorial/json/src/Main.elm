-- let us do something with json
-- todo items at https://jsonplaceholder.typicode.com/todos/1
-- show the todo item in a neat ui
-- the ui has to be dead simple of course

module Main exposing (main)

import Http exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)

main =
        Browser.elements { init = init, update = update, subscriptions = subscriptions, view = view}

type Model = 
        Failure
        | Loading
        | Success TodoItem

type Msg = 
        GetTodoItem (Result Http.Error TodoItem)

type alias TodoItem = 
        { userId : Int
        , itemNumber : Int
        , title : String
        , completed : Bool
        }

init : () -> (Msg, Cmd Msg)
init _ = 
        ( Loading, getTodoItem )

update msg model =
        case msg of
                GetTodoItem str ->
                        case str of
                                Ok todoItem ->  


