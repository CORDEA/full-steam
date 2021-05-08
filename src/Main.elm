module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (href)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, at, field)
import String
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
    , page : Page
    , homeData : HomeData
    }


type Page
    = Home
    | Detail


type HomeData
    = Failure
    | Loading
    | Success Apps


type alias Apps =
    List App


type alias App =
    { id : Int, name : String }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key Home Loading, fetchApps )


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
            ( model, Cmd.none )

        AppsFetched result ->
            case result of
                Ok value ->
                    ( { model | homeData = Success value }, Cmd.none )

                Err _ ->
                    ( { model | homeData = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ case model.page of
            Home ->
                home model.homeData

            Detail ->
                div [] []
        ]
    }


home : HomeData -> Html msg
home data =
    div []
        [ case data of
            Success value ->
                ul [] (List.map item value)

            Failure ->
                text "Failed to fetch apps"

            Loading ->
                text "Loading..."
        ]


item : App -> Html msg
item app =
    li [] [ a [ href (String.append "/apps/" (String.fromInt app.id)) ] [ text app.name ] ]



-- DECODER


appsDecoder : Decoder Apps
appsDecoder =
    at [ "applist", "apps" ]
        (Decode.list
            (Decode.map2 App
                (field "appid" Decode.int)
                (field "name" Decode.string)
            )
        )
