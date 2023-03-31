module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import OAuth
import OAuth.AuthorizationCode as OAuth
import Process
import Random
import Random.List
import Task
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
    , songs : List Song
    }


type Msg
    = GotAccessToken (Result Http.Error OAuth.AuthenticationSuccess)
    | Noop
    | Username (Result Http.Error String)
    | Playlists (Result Http.Error (List Playlist))
    | Shuffle Playlist
    | Songs (Result Http.Error ( List Song, Maybe String ))
    | ShuffledState (List Song)
    | AddedToQueue


type alias Playlist =
    { name : String
    , id : String
    , length : Int
    }


type alias Song =
    { name : String
    , uri : String
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
            { authToken = "", key = key, username = "", playlists = [], songs = [] }
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
            ( { model | playlists = p :: List.filter ((/=) p) model.playlists, songs = [] }
            , get ("/playlists/" ++ p.id ++ "/tracks") (Http.expectJson Songs songsDecoder) model.authToken
            )

        Songs (Ok ( songs, nextQuery )) ->
            let
                m =
                    { model | songs = model.songs ++ songs }
            in
            ( m
            , case nextQuery of
                Nothing ->
                    Random.generate ShuffledState (Random.List.shuffle m.songs)

                Just fullUrl ->
                    let
                        path =
                            String.dropLeft (String.length (Url.toString apiUrl)) fullUrl
                    in
                    get path (Http.expectJson Songs songsDecoder) model.authToken
            )

        ShuffledState songs ->
            let
                makeTask song =
                    postTask ("/me/player/queue?uri=" ++ song.uri) model.authToken
                        |> Task.andThen (\_ -> Process.sleep 100000000)
                        |> Task.perform (\_ -> AddedToQueue)
            in
            ( { model | songs = songs }
            , Cmd.batch (List.map makeTask (List.take 100 songs))
            )

        fail ->
            let
                _ =
                    Debug.log "fail msg" msg
            in
            ( model, Cmd.none )



-- queue : Model -> Cmd Msg


getSongs : Playlist -> String -> Cmd Msg
getSongs { id } tok =
    get ("/playlists/" ++ id ++ "/tracks") (Http.expectJson Songs songsDecoder) tok



-- TODO: Handle playlists with songs whose id is null


songsDecoder : Decode.Decoder ( List Song, Maybe String )
songsDecoder =
    let
        fields =
            Decode.map2 Song (Decode.field "name" Decode.string) (Decode.field "uri" Decode.string)

        songs =
            Decode.at [ "items" ] (Decode.list (Decode.at [ "track" ] fields))

        nextUrl =
            Decode.field "next" (Decode.nullable Decode.string)
    in
    Decode.map2 (\x y -> ( x, y )) songs nextUrl


authUrl : Url.Url
authUrl =
    OAuth.makeAuthorizationUrl
        { clientId = clientId
        , url = { defaultHttpsUrl | host = "accounts.spotify.com", path = "/authorize" }
        , redirectUri = homeUrl
        , scope = [ "playlist-read-private", "user-modify-playback-state" ]
        , state = Nothing
        }


getUsername : String -> Cmd Msg
getUsername =
    get "/me" (Http.expectJson Username (Decode.field "id" Decode.string))


getPlaylists : String -> Cmd Msg
getPlaylists =
    get "/me/playlists" (Http.expectJson Playlists playlistsDecoder)


playlistsDecoder : Decode.Decoder (List Playlist)
playlistsDecoder =
    let
        fields =
            Decode.map3
                Playlist
                (Decode.field "name" Decode.string)
                (Decode.field "id" Decode.string)
                (Decode.at [ "tracks" ] (Decode.field "total" Decode.int))
    in
    Decode.at [ "items" ] (Decode.list fields)


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


post : String -> String -> Cmd Msg
post path tok =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" tok ]
        , url = Url.toString { apiUrl | path = path }
        , body = Http.emptyBody
        , expect = Http.expectWhatever (\_ -> AddedToQueue)
        , timeout = Nothing
        , tracker = Nothing
        }


postTask : String -> String -> Task.Task Never Msg
postTask path tok =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" tok ]
        , url = Url.toString { apiUrl | path = path }
        , body = Http.emptyBody
        , resolver = Http.stringResolver (\_ -> Ok Noop)
        , timeout = Nothing
        }


req : { path : String, token : String, method : String, expect : Http.Expect Msg } -> Cmd Msg
req args =
    Http.request
        { method = args.method
        , headers = [ Http.header "Authorization" args.token ]
        , url = Url.toString { apiUrl | path = args.path }
        , body = Http.emptyBody
        , expect = args.expect
        , timeout = Nothing
        , tracker = Nothing
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
        , ul [] (List.map (\s -> li [] [ text s.name ]) model.songs)
        , text (String.fromInt (List.length model.songs))
        ]
    }


viewPlaylist : Playlist -> Html Msg
viewPlaylist p =
    li [] [ button [ onClick (Shuffle p) ] [ text p.name, text (String.fromInt p.length) ] ]



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
