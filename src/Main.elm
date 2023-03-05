module Main exposing (..)

import Base64
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import OAuth
import OAuth.AuthorizationCode as OAuth
import Url exposing (Protocol(..))
import Url.Builder as Bld
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query


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


baseUrl : String
baseUrl =
    "127.0.0.1:5500/index.html"


homeUrl : Url.Url
homeUrl =
    { defaultHttpUrl | host = "127.0.0.1:5500/index.html" }


basePath : String
basePath =
    "/index.html"


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

        OAuth.Success { code, state } ->
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



-- getAuthToken : String -> Cmd Msg
-- getAuthToken t =
--     Http.request
--         { method = "POST"
--         , headers = [ Http.header "Authorization" base64Code, Http.header "Content-Type" "application/x-www-form-urlencoded" ]
--         , url = "https://accounts.spotify.com/api/token"
--         , body =
--             Http.multipartBody
--                 [ Http.stringPart "code" t
--                 , Http.stringPart "grant_type" "client_credentials"
--                 -- , Http.stringPart "grant_type" "authorization_code"
--                 , Http.stringPart "redirect_uri" baseUrl
--                 ]
--         , expect = Http.expectString GotText
--         , timeout = Nothing
--         , tracker = Nothing
--         }


base64Code : String
base64Code =
    "Basic " ++ Base64.encode (clientId ++ ":" ++ clientSecret)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestedAuth ->
            ( model, auth |> OAuth.makeAuthorizationUrl |> Url.toString |> Nav.load )

        GotAccessToken (Ok a) ->
            ( { model | authToken = OAuth.tokenToString a.token }, Cmd.none )

        GotAccessToken (Err e) ->
            ( { model | authToken = "error boi" }, Cmd.none )


auth : OAuth.Authorization
auth =
    { clientId = clientId
    , url = { defaultHttpsUrl | host = "accounts.spotify.com", path = "/authorize" }
    , redirectUri = { defaultHttpUrl | host = baseUrl }
    , scope = [ "playlist-read-private", "user-modify-playback-state" ]
    , state = Nothing
    }



-- PARSING


getAccessCode : Url.Url -> String
getAccessCode url =
    Maybe.withDefault "" (Parser.parse authResult url)


authResult : Parser.Parser (String -> String) String
authResult =
    Parser.map (Maybe.withDefault "") (Parser.s "index.html" <?> Query.string "code")



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


defaultHttpsUrl : Url.Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


defaultHttpUrl : Url.Url
defaultHttpUrl =
    { protocol = Http
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }
