module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Url
import Url.Builder as Bld
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query
import String exposing (toList)
import List.Extra exposing (takeWhile)

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
init _ _ _ = ({token = ""}, Cmd.none)

-- getToken : Url.Url -> String
-- getToken url = Maybe.withDefault "" (Parser.parse authResult url)

-- -- authResult : Parser.Parser (String -> String) (Maybe String)
-- -- authResult = (Parser.s baseUrl <?> Query.string "code")

-- authResult : Parser.Parser (String -> String) String
-- authResult = Parser.s baseUrl <?> Query.string "code"

getToken : Url.Url -> String
getToken url = extract (Url.toString url)

extract : String -> String
extract x = String.reverse x |> String.toList |> takeWhile (\c -> c /= '?')

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
  , text model.token]


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


