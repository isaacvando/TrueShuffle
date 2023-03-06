module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import OAuth
import OAuth.AuthorizationCode as OAuth
import Url exposing (Protocol(..))
import Url.Parser exposing ((<?>))


main : Program () Model Msg
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
    , playlists : List Playlist
    }


type Msg
    = GotAccessToken (Result Http.Error OAuth.AuthenticationSuccess)
    | Noop
    | Username (Result Http.Error String)
    | Playlists (Result Http.Error (List Playlist))
    | Shuffle Playlist


type alias Playlist =
    { name : String
    }



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
            { authToken = "", key = key, username = "", playlists = [] }
    in
    case OAuth.parseCode url of
        OAuth.Success { code } ->
            ( model, getAuthToken code )

        _ ->
            ( model, Nav.load (Url.toString authUrl) )


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
        GotAccessToken (Ok a) ->
            let
                tok =
                    OAuth.tokenToString a.token
            in
            ( { model | authToken = tok }
            , Cmd.batch
                [ Nav.replaceUrl model.key (Url.toString homeUrl)
                , getUsername tok
                , getPlaylists tok
                ]
            )

        Username (Ok name) ->
            ( { model | username = name }, Cmd.none )

        Playlists (Ok playlists) ->
            ( { model | playlists = playlists }, Cmd.none )

        Shuffle p ->
            ( { model | playlists = p :: List.filter ((/=) p) model.playlists }, Cmd.none )

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
        [ h1 [] [ text "True Shuffle for Spotify" ]
        , text <| "Welcome, " ++ model.username ++ "!"
        , br [] []
        , ul [] (List.map viewPlaylist model.playlists)
        ]
    }


viewPlaylist : Playlist -> Html Msg
viewPlaylist p =
    li [] [ button [ onClick (Shuffle p) ] [ text p.name ] ]


getUsername : String -> Cmd Msg
getUsername =
    get "/me" (Http.expectJson Username (Json.field "id" Json.string))


getPlaylists : String -> Cmd Msg
getPlaylists =
    get "/me/playlists" (Http.expectJson Playlists playlistsDecoder)


playlistsDecoder : Json.Decoder (List Playlist)
playlistsDecoder =
    let
        single =
            Json.map Playlist (Json.field "name" Json.string)
    in
    Json.at [ "items" ] (Json.list single)


get : String -> Http.Expect Msg -> String -> Cmd Msg
get path expect tok =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" tok ]
        , url = Url.toString { apiUrl | path = path }
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }



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
