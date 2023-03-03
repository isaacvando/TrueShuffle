module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Url
import Url.Builder as Bld
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query

main = Browser.application
  {
    init = init,
    view = view,
    update = update, 
    subscriptions = subs,
    onUrlRequest = onUrlRequest,
    onUrlChange = onUrlChange
  }


type alias Model = 
  {
    token : String
  }

type Msg = RequestedAuth

baseUrl : String
baseUrl = "http://127.0.0.1:5500/index.html"


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ = ({token = getToken url}, Cmd.none)

getToken : Url.Url -> String
getToken url = Maybe.withDefault "getToken" (Parser.parse authResult url)

authResult : Parser.Parser (String -> String) String
authResult = Parser.map (Maybe.withDefault "auth") (Parser.s "index.html" <?> Query.string "code")

-- foo : String -> String
-- foo x = case Url.fromString x of
--   Nothing -> "foo"
--   Just url -> getToken url


view : Model -> Browser.Document Msg
view model = 
  {
  title = "True Shuffle"
  , body = viewBody model
  }


viewBody : Model -> List (Html Msg)
viewBody model = [
  h1 [] [text "True Shuffle for Spotify"]
  , button [onClick RequestedAuth] [text "Authenticate TrueShuffle"]
  , text <| "token: " ++ model.token]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  RequestedAuth -> (model, Nav.load authUrl)


authUrl : String
authUrl = Bld.crossOrigin "https://accounts.spotify.com/authorize" [] 
  [
  Bld.string "client_id" "0cdc0205c0184f809fa13c8f71d7848c"
  , Bld.string "redirect_uri" baseUrl
  , Bld.string "response_type" "code"
  ]




subs : Model -> Sub Msg
subs _ = Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ = RequestedAuth


onUrlChange : Url.Url -> Msg
onUrlChange _ = RequestedAuth
