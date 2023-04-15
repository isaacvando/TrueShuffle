port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Bytes.Encode as Bytes
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import OAuth
import OAuth.AuthorizationCode.PKCE as PKCE
import Random
import Random.List
import Url exposing (Protocol(..), Url)
import Url.Parser exposing ((<?>))


main : Program ( List Int, Encode.Value ) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> randomBytes GotRandomBytes
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }


port setStorage : Encode.Value -> Cmd a


port genRandomBytes : () -> Cmd msg


port randomBytes : (List Int -> msg) -> Sub msg


type alias Model =
    { authToken : String
    , key : Nav.Key
    , username : String
    , picture : String
    , playlists : List Playlist
    , errorState : Maybe Msg
    }


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


type Msg
    = GotAccessToken (Result Http.Error PKCE.AuthenticationSuccess)
    | NoOp
    | Error String
    | GotRandomBytes (List Int)
    | GotUser (Result Http.Error ( String, String ))
    | GotPlaylists (Result Http.Error ( List Playlist, Maybe String ))
    | ClickedShuffle Playlist
    | GotSongs Playlist (Result Http.Error ( List Song, Maybe String ))
    | ShuffledSongs Playlist
    | AddedToQueue Int (List Song)



-- CONSTANTS


homeUrl : Url.Url
homeUrl =
    { defaultHttpsUrl | host = "isaacvando.github.io", path = "/TrueShuffle" }



-- { defaultHttpsUrl | protocol = Http, host = "127.0.0.1:5500/index.html" }


apiUrl : Url.Url
apiUrl =
    { defaultHttpsUrl | host = "api.spotify.com/v1" }


clientId : String
clientId =
    "0cdc0205c0184f809fa13c8f71d7848c"



-- INIT


init : ( List Int, Encode.Value ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ( randomList, playlists ) url key =
    let
        p =
            Decode.decodeValue storageDecoder playlists |> Result.withDefault []

        model =
            { authToken = "", key = key, username = "", picture = "", playlists = p, errorState = Nothing }
    in
    case PKCE.parseCode url of
        PKCE.Success { code } ->
            case getCodeVerifier randomList of
                Just c ->
                    ( model, getAuthToken code c )

                Nothing ->
                    update (Error "After recieving an access code a codeVerifier could not be constructed") model

        _ ->
            ( model, genRandomBytes () )



-- AUTHENTICATION


getAuthUrl : PKCE.CodeVerifier -> Url
getAuthUrl codeVerifier =
    PKCE.makeAuthorizationUrl
        { clientId = clientId
        , url = { defaultHttpsUrl | host = "accounts.spotify.com", path = "/authorize" }
        , redirectUri = homeUrl
        , scope = [ "playlist-read-private", "user-modify-playback-state" ]
        , state = Nothing
        , codeChallenge = PKCE.mkCodeChallenge codeVerifier
        }


getCodeVerifier : List Int -> Maybe PKCE.CodeVerifier
getCodeVerifier x =
    List.map Bytes.unsignedInt8 x
        |> Bytes.sequence
        |> Bytes.encode
        |> PKCE.codeVerifierFromBytes


getAuthToken : PKCE.AuthorizationCode -> PKCE.CodeVerifier -> Cmd Msg
getAuthToken code verifier =
    Http.request <|
        PKCE.makeTokenRequest GotAccessToken
            { credentials =
                { clientId = clientId
                , secret = Nothing
                }
            , code = code
            , url = { defaultHttpsUrl | host = "accounts.spotify.com", path = "/api/token" }
            , redirectUri = homeUrl
            , codeVerifier = verifier
            }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomBytes randomList ->
            case getCodeVerifier randomList of
                Nothing ->
                    update (Error "A codeVerifier could not be constructed") model

                Just c ->
                    ( model, getAuthUrl c |> Url.toString |> Nav.load )

        GotAccessToken (Ok a) ->
            let
                tok =
                    OAuth.tokenToString a.token
            in
            ( { model | authToken = tok }
            , Cmd.batch
                [ Nav.replaceUrl model.key (Url.toString homeUrl)
                , getUser tok
                , get (getUrlFromPath "/me/playlists") (Http.expectJson GotPlaylists playlistsDecoder) tok
                ]
            )

        GotUser (Ok ( name, image )) ->
            ( { model | username = name, picture = image }, Cmd.none )

        GotPlaylists (Ok ( playlists, nextUrl )) ->
            case nextUrl of
                Nothing ->
                    ( { model | playlists = keepUnchanged model.playlists playlists }, Cmd.none )

                Just url ->
                    ( { model | playlists = model.playlists ++ playlists }
                    , get url (Http.expectJson GotPlaylists playlistsDecoder) model.authToken
                    )

        ClickedShuffle p ->
            ( model
            , if p.songs == [] then
                getTracks p model.authToken

              else
                enqueuePlaylist p
            )

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
                        [ enqueuePlaylist newP
                        , setStorage (storageEncoder m.playlists)
                        ]

                Just url ->
                    get url (Http.expectJson (GotSongs newP) songsDecoder) model.authToken
            )

        AddedToQueue count songs ->
            case songs of
                x :: xs ->
                    if count == List.length songs || count == 100 then
                        ( model, Cmd.none )

                    else
                        ( model
                        , post
                            (getUrlFromPath ("/me/player/queue?uri=" ++ x.uri))
                            model.authToken
                            (\_ -> AddedToQueue (count + 1) (xs ++ [ x ]))
                        )

                [] ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        _ ->
            let
                _ =
                    Debug.log "Error" msg
            in
            ( { model | errorState = Just msg }, Cmd.none )


enqueuePlaylist : Playlist -> Cmd Msg
enqueuePlaylist p =
    Random.generate (AddedToQueue 0) (Random.List.shuffle p.songs)


getTracks : Playlist -> String -> Cmd Msg
getTracks p token =
    get (getUrlFromPath <| "/playlists/" ++ p.id ++ "/tracks") (Http.expectJson (GotSongs p) songsDecoder) token


keepUnchanged : List Playlist -> List Playlist -> List Playlist
keepUnchanged old new =
    let
        replace p =
            case List.filter (\x -> x.id == p.id) new of
                [] ->
                    p

                x :: _ ->
                    if x.snapshot == p.snapshot then
                        x

                    else
                        p
    in
    List.map replace old


getUser : String -> Cmd Msg
getUser =
    get (getUrlFromPath "/me") <|
        Http.expectJson GotUser <|
            Decode.map2 (\x y -> ( x, y ))
                (Decode.field "id" Decode.string)
                (Decode.at [ "images" ] (Decode.index 0 (Decode.field "url" Decode.string)))



-- VIEW


accentColor : Color
accentColor =
    rgb255 30 215 96


backgroundColor : Color
backgroundColor =
    rgb255 25 20 20


textColor : Color
textColor =
    rgb 1 1 1


view : Model -> Browser.Document Msg
view model =
    { title = "True Shuffle"
    , body =
        [ layout
            [ Background.color backgroundColor
            , Font.color textColor
            , Font.family
                [ Font.external
                    { name = "Roboto"
                    , url = "https://fonts.googleapis.com/css?family=Roboto"
                    }
                , Font.sansSerif
                ]
            , padding 15
            ]
            (case model.errorState of
                Nothing ->
                    viewBody model

                Just err ->
                    case err of
                        Error msg ->
                            viewError msg

                        _ ->
                            viewError "Whoops. Didn't expect that. Your best bet is checking dev tools ->"
            )
        ]
    }


viewBody model =
    column [ width fill, Font.center ]
        [ viewHeader model
        , column [ centerX, spacing 7 ] (List.map viewPlaylist model.playlists)
        ]


viewError msg =
    column [ width fill, Font.center, Font.color accentColor, spacing 10 ]
        [ paragraph [ Font.bold, Font.size 50 ] [ text "Ruh roh, error time" ]
        , paragraph [] [ text msg ]
        ]


viewPlaylist p =
    Input.button
        [ Background.color accentColor
        , Font.color backgroundColor
        , width (minimum 200 fill)
        , Border.rounded 50
        , height (px 40)
        , Font.center
        ]
        { onPress = Just (ClickedShuffle p)
        , label = paragraph [ padding 15, moveUp 1.5 ] [ text p.name ]
        }


viewHeader model =
    row
        [ centerX
        , Font.color accentColor
        , width (maximum 800 fill)
        , Font.size 30
        ]
        [ image [ width (px 67) ] { src = "../images/icon.png", description = "spotify logo" }
        , text "True Shuffle"
        , row
            [ alignRight
            , spacing 10
            ]
            [ text model.username
            , viewPfp model
            ]
        ]


viewPfp model =
    image
        [ centerX
        , centerY
        , Border.rounded 100
        , clip
        , width (px 50)
        ]
        { src = model.picture, description = "User profile picture" }



-- ENCODERS/DECODERS


songsDecoder : Decode.Decoder ( List Song, Maybe String )
songsDecoder =
    let
        songs =
            Decode.at [ "items" ]
                (Decode.list
                    (Decode.at [ "track" ] <|
                        Decode.map2
                            Song
                            (Decode.field "name" Decode.string)
                            (Decode.field "uri" Decode.string)
                    )
                )

        nextUrl =
            Decode.field "next" (Decode.nullable Decode.string)
    in
    Decode.map2 (\x y -> ( x, y )) songs nextUrl


playlistsDecoder : Decode.Decoder ( List Playlist, Maybe String )
playlistsDecoder =
    let
        playlists =
            Decode.at [ "items" ]
                (Decode.list <|
                    Decode.map4
                        (Playlist [])
                        (Decode.field "name" Decode.string)
                        (Decode.field "id" Decode.string)
                        (Decode.at [ "tracks" ] (Decode.field "total" Decode.int))
                        (Decode.field "snapshot_id" Decode.string)
                )

        nextUrl =
            Decode.field "next" (Decode.nullable Decode.string)
    in
    Decode.map2 (\x y -> ( x, y )) playlists nextUrl


storageDecoder : Decode.Decoder (List Playlist)
storageDecoder =
    Decode.list <|
        Decode.map5
            Playlist
            (Decode.field "songs" <|
                Decode.list <|
                    Decode.map2
                        Song
                        (Decode.field "name" Decode.string)
                        (Decode.field "uri" Decode.string)
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


get : String -> Http.Expect Msg -> String -> Cmd Msg
get url expect tok =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" tok ]
        , url = url
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


post : String -> String -> (Result Http.Error () -> Msg) -> Cmd Msg
post url tok f =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" tok ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever f
        , timeout = Nothing
        , tracker = Nothing
        }


getUrlFromPath : String -> String
getUrlFromPath path =
    { apiUrl | path = path } |> Url.toString
