module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import OAuth
import OAuth.AuthorizationCode as OAuth
import Url exposing (Protocol(..))
import Url.Parser exposing ((<?>))


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type alias Model =
    { authToken : String
    }


type Msg
    = RequestedAuth
    | GotAccessToken (Result Http.Error OAuth.AuthenticationSuccess)



-- CONSTANTS


homeUrl : Url.Url
homeUrl =
    { defaultHttpsUrl | protocol = Http, host = "127.0.0.1:5500/index.html" }


clientSecret : String
clientSecret =
    "c14d1577b0e647d481170aa6192021f0"


clientId : String
clientId =
    "0cdc0205c0184f809fa13c8f71d7848c"



-- INIT


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    case OAuth.parseCode url of
        OAuth.Empty ->
            ( { authToken = "" }, Cmd.none )

        OAuth.Success { code } ->
            ( { authToken = "" }, getAuthToken code )

        _ ->
            ( { authToken = "" }, Cmd.none )


getAuthToken : OAuth.AuthorizationCode -> Cmd Msg
getAuthToken code =
    Http.request <|
        OAuth.makeTokenRequest GotAccessToken
            { credentials =
                { clientId = clientId
                , secret = Just clientSecret
                }
            , code = code
            , url = { defaultHttpsUrl | host = "accounts.spotify.com", path = "/api/token" }
            , redirectUri = homeUrl
            }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestedAuth ->
            ( model, auth |> OAuth.makeAuthorizationUrl |> Url.toString |> Nav.load )

        GotAccessToken (Ok a) ->
            ( { model | authToken = OAuth.tokenToString a.token }, Cmd.none )

        GotAccessToken (Err _) ->
            ( { model | authToken = "error boi" }, Cmd.none )


auth : OAuth.Authorization
auth =
    { clientId = clientId
    , url = { defaultHttpsUrl | host = "accounts.spotify.com", path = "/authorize" }
    , redirectUri = homeUrl
    , scope = [ "playlist-read-private", "user-modify-playback-state" ]
    , state = Nothing
    }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "True Shuffle"
    , body = viewLogin model
    }


viewLogin : Model -> List (Html Msg)
viewLogin model =
    [ h1 [] [ text "True Shuffle for Spotify" ]
    , button [ onClick RequestedAuth ] [ text "Authenticate TrueShuffle" ]
    , br [] []
    , text <| "authToken: " ++ model.authToken
    ]



-- SUBS


subs : Model -> Sub Msg
subs _ =
    Sub.none



-- URL


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    RequestedAuth


onUrlChange : Url.Url -> Msg
onUrlChange _ =
    RequestedAuth



-- HELPERS


defaultHttpsUrl : Url.Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }
