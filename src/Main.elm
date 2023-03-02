module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Url
import Browser.Navigation as Nav
import Url.Builder as Bld

main = Browser.application
  {
    init = init,
    view = view,
    update = update, 
    subscriptions = subs,
    onUrlRequest = onUrlRequest,
    onUrlChange = onUrlChange
  }


type alias Model = {}

type Msg = RequestedAuth


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ = ({}, Cmd.none)


view : Model -> Browser.Document Msg
view model = 
  {
  title = "True Shuffle"
  , body = viewBody model
  }


viewBody : Model -> List (Html Msg)
viewBody _ = [
  h1 [] [text "True Shuffle for Spotify"]
  , button [onClick RequestedAuth] [text "Authenticate TrueShuffle"]]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  RequestedAuth -> (model, Nav.load authUrl)


authUrl : String
authUrl = Bld.crossOrigin "https://accounts.spotify.com/authorize" [] 
  [
  Bld.string "client_id" "0cdc0205c0184f809fa13c8f71d7848c"
  , Bld.string "redirect_uri" "http://127.0.0.1:5500/index.html"
  , Bld.string "response_type" "code"
  ]




subs : Model -> Sub Msg
subs _ = Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ = RequestedAuth


onUrlChange : Url.Url -> Msg
onUrlChange _ = RequestedAuth


