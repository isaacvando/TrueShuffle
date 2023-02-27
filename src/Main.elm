module Main exposing (..)

import Browser exposing (Document, document)
import Html exposing (h1, text, div)

main = document
  {
    init = init,
    view = view,
    update = update, 
    subscriptions = subscriptions
  }


type alias Model = {}

type Msg = Msg


init : () -> (Model, Cmd Msg)
init _ = ({}, Cmd.none)

view : Model -> Document Msg
view _ = 
  {
  title = "True Shuffle"
  , body = [h1 [] [text "True Shuffle for Spotify"]]
  }

update : Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
