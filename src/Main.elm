port module Main exposing (..)

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


main : Program Encode.Value Model Msg
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
    | NoOp
    | GotUsername (Result Http.Error String)
    | GotPlaylists (Result Http.Error (List Playlist))
    | ClickedShuffle Playlist
    | GotSongs Playlist (Result Http.Error ( List Song, Maybe String ))
    | ShuffledSongs Playlist
    | AddedToQueue Int (List Song)


type alias Playlist =
    { songs : List Song
    , name : String
    , id : String
    , length : Int
    , snapshot : String
    }


type alias Song =
    { name : String
    , uri : String
    }


port setStorage : Encode.Value -> Cmd a



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


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        p =
            case Decode.decodeValue storageDecoder flags of
                Ok x ->
                    x

                Err _ ->
                    []

        model =
            { authToken = "", key = key, username = "", playlists = p }
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

        GotUsername (Ok name) ->
            ( { model | username = name }, Cmd.none )

        GotPlaylists (Ok playlists) ->
            ( { model | playlists = keepUnchanged model.playlists playlists }, Cmd.none )

        ClickedShuffle p ->
            if p.songs == [] then
                ( model, get ("/playlists/" ++ p.id ++ "/tracks") (Http.expectJson (GotSongs p) songsDecoder) model.authToken )

            else
                update (GotSongs p (Ok ( [], Nothing ))) model

        GotSongs p (Ok ( songs, nextQuery )) ->
            let
                newP =
                    { p | songs = p.songs ++ songs }

                m =
                    { model | playlists = newP :: List.filter (\x -> newP.id /= x.id) model.playlists }
            in
            ( m
            , case nextQuery of
                Nothing ->
                    Cmd.batch
                        [ Random.generate (AddedToQueue 0) (Random.List.shuffle newP.songs)
                        , setStorage (storageEncoder m.playlists)
                        ]

                Just fullUrl ->
                    let
                        path =
                            String.dropLeft (String.length (Url.toString apiUrl)) fullUrl
                    in
                    get path (Http.expectJson (GotSongs newP) songsDecoder) model.authToken
            )

        AddedToQueue count songs ->
            case songs of
                x :: xs ->
                    if count == List.length songs || count == 5 then
                        ( model, Cmd.none )

                    else
                        ( model
                        , post
                            ("/me/player/queue?uri=" ++ x.uri)
                            model.authToken
                            (\_ -> AddedToQueue (count + 1) (xs ++ [ x ]))
                        )

                [] ->
                    ( model, Cmd.none )

        _ ->
            let
                _ =
                    Debug.log "Error" msg
            in
            ( model, Cmd.none )


removePlaylistById : String -> List Playlist -> List Playlist
removePlaylistById id =
    List.filter (\p -> p.id /= id)


keepUnchanged : List Playlist -> List Playlist -> List Playlist
keepUnchanged old new =
    let
        replace p =
            case List.filter (\x -> x.id == p.id) old of
                [] ->
                    p

                x :: _ ->
                    if x.snapshot == p.snapshot then
                        x

                    else
                        p
    in
    List.map replace new



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
    get "/me" (Http.expectJson GotUsername (Decode.field "id" Decode.string))


getPlaylists : String -> Cmd Msg
getPlaylists =
    get "/me/playlists" (Http.expectJson GotPlaylists playlistsDecoder)


playlistsDecoder : Decode.Decoder (List Playlist)
playlistsDecoder =
    let
        fields =
            Decode.map4
                (Playlist [])
                (Decode.field "name" Decode.string)
                (Decode.field "id" Decode.string)
                (Decode.at [ "tracks" ] (Decode.field "total" Decode.int))
                (Decode.field "snapshot_id" Decode.string)
    in
    Decode.at [ "items" ] (Decode.list fields)


storageDecoder : Decode.Decoder (List Playlist)
storageDecoder =
    Decode.list <|
        Decode.map5
            Playlist
            (Decode.list <|
                Decode.map2
                    Song
                    (Decode.field "name" Decode.string)
                    (Decode.field "id" Decode.string)
            )
            (Decode.field "name" Decode.string)
            (Decode.field "id" Decode.string)
            (Decode.field "length" Decode.int)
            (Decode.field "snapshot" Decode.string)


storageEncoder : List Playlist -> Encode.Value
storageEncoder ps =
    let
        encodeSong s =
            Encode.object
                [ ( "name", Encode.string s.name )
                , ( "uri", Encode.string s.uri )
                ]

        item p =
            Encode.object
                [ ( "songs", Encode.list encodeSong p.songs )
                , ( "name", Encode.string p.name )
                , ( "id", Encode.string p.id )
                , ( "length", Encode.int p.length )
                , ( "snapshot", Encode.string p.snapshot )
                ]
    in
    Encode.list item ps


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


post : String -> String -> (Result Http.Error () -> Msg) -> Cmd Msg
post path tok f =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" tok ]
        , url = Url.toString { apiUrl | path = path }
        , body = Http.emptyBody
        , expect = Http.expectWhatever f
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
        , resolver = Http.stringResolver (\_ -> Ok NoOp)
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
        ]
    }


viewPlaylist : Playlist -> Html Msg
viewPlaylist p =
    li [] [ button [ onClick (ClickedShuffle p) ] [ text p.name ], text "  ", text (String.fromInt p.length) ]



-- SUBS


subs : Model -> Sub Msg
subs _ =
    Sub.none



-- URL


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url.Url -> Msg
onUrlChange _ =
    NoOp



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
