module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (href)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, at, field)
import String
import Url
import Url.Parser as Parser exposing ((</>), Parser)



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
    , homeData : Data Apps
    , newsData : Data AppNews
    }


type Page
    = Home
    | Detail { id : Int }


type Data data
    = Failure
    | Loading
    | Success data


type alias Apps =
    List App


type alias App =
    { id : Int, name : String }


type alias AppNews =
    List AppNewsItem


type alias AppNewsItem =
    { title : String, url : String, contents : String }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        page =
            detectPage url
    in
    ( Model key page Loading Loading
    , case page of
        Home ->
            fetchApps

        Detail id ->
            fetchAppNews id.id
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AppsFetched (Result Http.Error Apps)
    | AppNewsFetched (Result Http.Error AppNews)


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
            let
                page =
                    detectPage url
            in
            ( { model | page = page }
            , case page of
                Home ->
                    fetchApps

                Detail id ->
                    fetchAppNews id.id
            )

        AppsFetched result ->
            case result of
                Ok value ->
                    ( { model | homeData = Success value }, Cmd.none )

                Err _ ->
                    ( { model | homeData = Failure }, Cmd.none )

        AppNewsFetched result ->
            case result of
                Ok value ->
                    ( { model | newsData = Success value }, Cmd.none )

                Err _ ->
                    ( { model | newsData = Failure }, Cmd.none )


detectPage : Url.Url -> Page
detectPage url =
    case Parser.parse urlParser url of
        Just id ->
            Detail { id = id }

        Nothing ->
            Home


urlParser : Parser (Int -> a) a
urlParser =
    Parser.s "apps" </> Parser.int


fetchApps : Cmd Msg
fetchApps =
    Http.get
        { url = ""
        , expect = Http.expectJson AppsFetched appsDecoder
        }


fetchAppNews : Int -> Cmd Msg
fetchAppNews id =
    Http.get
        { url = ""
        , expect = Http.expectJson AppNewsFetched appNewsDecoder
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Full steam"
    , body =
        [ case model.page of
            Home ->
                home model.homeData

            Detail id ->
                news model.newsData
        ]
    }


home : Data Apps -> Html Msg
home data =
    div []
        [ case data of
            Success value ->
                ul [] (List.map homeItem value)

            Failure ->
                text "Failed to fetch apps"

            Loading ->
                text "Loading..."
        ]


homeItem : App -> Html Msg
homeItem app =
    li [] [ a [ href (String.append "/apps/" (String.fromInt app.id)) ] [ text app.name ] ]


news : Data AppNews -> Html Msg
news data =
    div []
        [ case data of
            Success value ->
                ul [] (List.map newsItem value)

            Failure ->
                text "Failed to fetch app news"

            Loading ->
                text "Loading..."
        ]


newsItem : AppNewsItem -> Html Msg
newsItem item =
    li [] [ a [ href item.url ] [ text item.title ] ]



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


appNewsDecoder : Decoder AppNews
appNewsDecoder =
    at [ "appnews", "newsitems" ]
        (Decode.list
            (Decode.map3 AppNewsItem
                (field "title" Decode.string)
                (field "url" Decode.string)
                (field "contents" Decode.string)
            )
        )
