module Main exposing (..)

import Base64
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Url
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
    { accessCode : String
    , authToken : String
    , authorized : Bool
    }


type Msg
    = RequestedAuth
    | GetStuff
    | GotStuff
    | AuthToken String
    | GotText (Result Http.Error String)



-- CONSTANTS


baseUrl : String
baseUrl =
    "http://127.0.0.1:5500/index.html"


clientSecret : String
clientSecret =
    "c14d1577b0e647d481170aa6192021f0"


clientId : String
clientId =
    "0cdc0205c0184f809fa13c8f71d7848c"



-- INIT


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    if getAccessCode url == "" then
        ( { accessCode = "", authorized = False, authToken = "" }, Cmd.none )

    else
        ( { accessCode = getAccessCode url, authorized = False, authToken = "" }, getAuthToken (getAccessCode url) )


getAuthToken : String -> Cmd Msg
getAuthToken t =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" base64Code, Http.header "Content-Type" "application/x-www-form-urlencoded" ]
        , url = "https://accounts.spotify.com/api/token"
        , body =
            Http.multipartBody
                [ Http.stringPart "code" t
                , Http.stringPart "grant_type" "authorization_code"
                , Http.stringPart "redirect_uri" baseUrl
                ]
        , expect = Http.expectString GotText
        , timeout = Nothing
        , tracker = Nothing
        }


base64Code : String
base64Code =
    clientId ++ ":" ++ clientSecret |> Base64.encode



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestedAuth ->
            ( model, Nav.load authUrl )

        GetStuff ->
            ( model
            , Http.request
                { method = "GET"
                , headers = [ Http.header "Authorization" model.accessCode, Http.header "Content-Type" "application/json" ]
                , url = "https://api.spotify.com/v1/me"
                , body = Http.emptyBody
                , expect = Http.expectWhatever (\_ -> GotStuff)
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        GotStuff ->
            ( model, Cmd.none )

        AuthToken s ->
            ( { model | authToken = s, authorized = True }, Cmd.none )

        GotText r ->
            case r of
                Ok s ->
                    ( { model | authToken = s }, Cmd.none )

                _ ->
                    ( { model | authToken = "bonk bonk error moment" }, Cmd.none )


authUrl : String
authUrl =
    Bld.crossOrigin "https://accounts.spotify.com/authorize"
        []
        [ Bld.string "client_id" clientId
        , Bld.string "redirect_uri" baseUrl
        , Bld.string "response_type" "code"
        , Bld.string "scope" "playlist-read-private user-modify-playback-state"
        ]



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
    , text <| "accessCode: " ++ model.accessCode
    , br [] []
    , text <| "authToken: " ++ model.authToken
    ]


viewHome : Model -> List (Html Msg)
viewHome model =
    [ text <| "token: " ++ model.accessCode
    , br [] []
    , button [ onClick GetStuff ] [ text "retrieve something" ]
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
