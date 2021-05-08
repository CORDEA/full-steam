module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (text)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, at, field, map2)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , data : AppModel
    }


type AppModel
    = Failure
    | Loading
    | Success Apps


type alias Apps =
    List App


type alias App =
    { id : Int, name : String }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url Loading, fetchApps )


fetchApps : Cmd Msg
fetchApps =
    Http.get
        { url = ""
        , expect = Http.expectJson AppsFetched appsDecoder
        }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AppsFetched (Result Http.Error Apps)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        AppsFetched result ->
            case result of
                Ok value ->
                    ( { model | data = Success value }, Cmd.none )

                Err _ ->
                    ( { model | data = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body = [ text (Url.toString model.url) ]
    }



-- DECODER


appsDecoder : Decoder Apps
appsDecoder =
    at [ "applist", "apps" ]
        (Decode.list
            (map2 App
                (field "appid" Decode.int)
                (field "name" Decode.string)
            )
        )
