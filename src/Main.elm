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
    , key : Nav.Key
    , username : String
    }


type Msg
    = RequestedAuth
    | GotAccessToken (Result Http.Error OAuth.AuthenticationSuccess)
    | Noop
    | Fetch (Cmd Msg)
    | Username String



-- CONSTANTS


homeUrl : Url.Url
homeUrl =
    { defaultHttpsUrl | protocol = Http, host = "127.0.0.1:5500/index.html" }


apiUrl : Url.Url
apiUrl =
    { defaultHttpsUrl | host = "api.spotify.com/v1" }


clientSecret : String
clientSecret =
    "c14d1577b0e647d481170aa6192021f0"


clientId : String
clientId =
    "0cdc0205c0184f809fa13c8f71d7848c"



-- INIT


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { authToken = "", key = key, username = "" }
    in
    case OAuth.parseCode url of
        OAuth.Success { code } ->
            ( model, getAuthToken code )

        _ ->
            ( model, Cmd.none )


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
            ( model, Nav.load (Url.toString authUrl) )

        GotAccessToken (Ok a) ->
            ( { model | authToken = OAuth.tokenToString a.token }
            , Nav.replaceUrl model.key (Url.toString homeUrl)
            )

        Fetch cmd ->
            ( model, cmd )

        Username name ->
            ( { model | username = name }, Cmd.none )

        _ ->
            ( model, Cmd.none )


authUrl : Url.Url
authUrl =
    OAuth.makeAuthorizationUrl
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
    , body =
        if model.authToken == "" then
            viewLogin model

        else
            viewHome model
    }


viewLogin : Model -> List (Html Msg)
viewLogin model =
    [ h1 [] [ text "True Shuffle for Spotify" ]
    , button [ onClick RequestedAuth ] [ text "Authenticate TrueShuffle" ]
    , br [] []
    , text <| "authToken: " ++ model.authToken
    ]


viewHome : Model -> List (Html Msg)
viewHome model =
    [ h1 [] [ text "True Shuffle for Spotify" ]
    , button [ onClick (Fetch (getUsername model.authToken)) ] [ text "fetch username" ]
    , br [] []
    , text model.username
    ]


getUsername : String -> Cmd Msg
getUsername tok =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" tok ]
        , url = Url.toString { apiUrl | path = "/me" }
        , body = Http.emptyBody
        , expect = Http.expectString foo
        , timeout = Nothing
        , tracker = Nothing
        }


foo r =
    case r of
        Ok s ->
            Username s

        Err _ ->
            Username "there was an error"



-- SUBS


subs : Model -> Sub Msg
subs _ =
    Sub.none



-- URL


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    Noop


onUrlChange : Url.Url -> Msg
onUrlChange _ =
    Noop



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
