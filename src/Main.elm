module Main exposing (..)

import Html exposing (Html, text, div, h1, img, header, section, span, main_)
import Html.Attributes exposing (attribute)
import Http
import Dict
import Navigation exposing (Location)
import Window
import Task


---- APPS ----

import Pages.Home as Home
import Utils.MDCClass as MDCClass
import Route exposing (Route)
import Device exposing (Device)
import Data.Songs as SongsData
import Ports
import Views


---- PAGE ----


type Page
    = Blank
    | NotFound
    | Errored
    | Home
    | Player


type PageState
    = Loading
    | Loaded Page



---- MODEL ----


type alias Model =
    { navOpen : Bool
    , device : Device
    , songs : SongsData.Songs
    , pageState : PageState
    }


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute (Route.fromLocation location)
        { navOpen = False
        , device = Device.classifyDevice <| Window.Size 0 0
        , songs = Dict.empty
        , pageState = Loading
        }



---- UPDATE ----


type Msg
    = SetRoute (Maybe Route)
    | WindowResize Window.Size
    | PortMsg Ports.JSDataIn
    | PortErr String
    | HomeLoaded (Result Http.Error SongsData.Songs)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.pageState msg model


updatePage : PageState -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    case ( msg, page ) of
        ( SetRoute route, _ ) ->
            let
                ( newModel, newCmd ) =
                    setRoute route model
            in
                newModel ! [ Task.perform WindowResize Window.size, newCmd ]

        ( WindowResize size, _ ) ->
            { model | device = Device.classifyDevice size } ! []

        ( HomeLoaded (Ok songs), _ ) ->
            { model | pageState = Loaded Home, songs = Dict.union songs model.songs }
                ! [ Ports.pushDataToJS Ports.HomePageLoaded ]

        ( HomeLoaded (Err errMessage), _ ) ->
            -- { model | pageState = Loaded (Errored <| Errored.pageLoadError <| toString errMessage) } ! []
            { model | pageState = Loaded Errored } ! []

        ( _, Loaded NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            model ! []

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            model ! []



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowResize
        , pageSubscriptions model
        , Ports.pullJSDataToElm PortMsg PortErr
        ]


pageSubscriptions : { a | pageState : PageState } -> Sub Msg
pageSubscriptions { pageState } =
    Sub.none



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.pageState of
        Loading ->
            Views.frame <| Views.loading

        Loaded Errored ->
            div []
                [ text "Errored"
                ]

        Loaded Home ->
            Views.frame <|
                Home.view model.songs

        Loaded Player ->
            Views.frame <| Views.loading

        Loaded Blank ->
            div []
                [ text "Blank"
                ]

        Loaded NotFound ->
            div []
                [ text "NotFound"
                ]


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } ! []

        Just Route.Root ->
            model ! [ Route.modifyUrl Route.Home ]

        Just Route.Home ->
            let
                ( pageState, cmd ) =
                    if (Dict.isEmpty model.songs) then
                        ( Loading, Task.attempt HomeLoaded Home.load )
                    else
                        ( Loaded Home, Cmd.none )
            in
                { model | navOpen = False, pageState = pageState }
                    ! [ cmd ]

        Just (Route.Player youTubeID) ->
            { model | pageState = Loaded Player } ! []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
