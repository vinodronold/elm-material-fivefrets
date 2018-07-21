module Main exposing (..)

import Html exposing (Html, text, div, h1, img, header, section, span, main_)
import Http
import Dict
import Navigation exposing (Location)
import Task


---- APPS ----

import Pages.Home as Home
import Pages.Player as Player
import Pages.Errored as Errored
import Route exposing (Route)
import Data.Songs as SongsData
import Data.ChordTime exposing (ChordTime)
import Ports
import Views
import Types


---- PAGE ----


type Page
    = Blank
    | NotFound
    | Errored Errored.PageLoadError
    | Home
    | Player Player.Model


type PageState
    = Loading
    | Loaded Page



---- MODEL ----


type alias Model =
    { navOpen : Bool
    , homeLoaded : Bool
    , songs : SongsData.Songs
    , pageState : PageState
    }


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute (Route.fromLocation location)
        { navOpen = False
        , homeLoaded = False
        , songs = Dict.empty
        , pageState = Loading
        }



---- UPDATE ----


type Msg
    = SetRoute (Maybe Route)
    | PortMsg Ports.JSDataIn
    | PortErr String
    | HomeLoaded (Result Http.Error SongsData.Songs)
    | PlayerChordsLoaded Types.YouTubeID (Result Http.Error (List ChordTime))
    | PlayerSongChordsLoaded Types.YouTubeID (Result Http.Error SongsData.Song)
    | PlayerMsg Player.Msg


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
                newModel ! [ newCmd ]

        ( HomeLoaded (Ok songs), _ ) ->
            { model | pageState = Loaded Home, songs = Dict.union songs model.songs }
                ! [ Ports.pushDataToJS Ports.HomeLoaded ]

        ( HomeLoaded (Err errMessage), _ ) ->
            -- { model | pageState = Loaded (Errored <| Errored.pageLoadError <| toString errMessage) } ! []
            { model | pageState = Loaded (Errored <| Errored.pageLoadError <| toString errMessage) } ! []

        ( PlayerChordsLoaded youTubeID (Ok chordtimes), _ ) ->
            let
                song =
                    Dict.get (Types.youTubeIDtoString youTubeID) model.songs

                ( pageState, songs ) =
                    case song of
                        Nothing ->
                            ( Loaded (Errored <| Errored.pageLoadError "Sorry. Unable to find the Song, please refresh the page.")
                            , model.songs
                            )

                        Just s ->
                            let
                                songUpdated =
                                    { s | chords = Just chordtimes }
                            in
                                ( Loaded (Player <| Player.init songUpdated)
                                , Dict.insert (Types.youTubeIDtoString youTubeID) songUpdated model.songs
                                )
            in
                { model | pageState = pageState, songs = songs }
                    ! [ Ports.pushDataToJS <| Ports.PlayerLoaded Types.ytPlayerID youTubeID
                      , Cmd.map PlayerMsg Player.getWindowSize
                      ]

        ( PlayerChordsLoaded youTubeID (Err errMessage), _ ) ->
            { model | pageState = Loaded (Errored <| Errored.pageLoadError <| toString errMessage) }
                ! []

        ( PlayerSongChordsLoaded youTubeID (Ok song), _ ) ->
            { model
                | pageState = Loaded (Player <| Player.init song)
                , songs = Dict.insert (Types.youTubeIDtoString youTubeID) song model.songs
            }
                ! [ Ports.pushDataToJS <| Ports.PlayerLoaded Types.ytPlayerID youTubeID
                  , Cmd.map PlayerMsg Player.getWindowSize
                  ]

        ( PlayerSongChordsLoaded youTubeID (Err errMessage), _ ) ->
            { model | pageState = Loaded (Errored <| Errored.pageLoadError <| toString errMessage) }
                ! []

        ( PlayerMsg playerMsg, Loaded (Player playerModel) ) ->
            let
                ( subModel, subCmd ) =
                    Player.update playerMsg playerModel
            in
                { model | pageState = Loaded (Player subModel) } ! [ Cmd.map PlayerMsg subCmd ]

        ( PortMsg jsDataIn, Loaded (Player playerModel) ) ->
            case jsDataIn of
                Ports.JSPlayerStatus jsPlayerStatus ->
                    let
                        ( subModel, subCmd ) =
                            Player.update
                                (Player.UpdatePlayerStatus <| Types.toPlayerStatus jsPlayerStatus)
                                playerModel
                    in
                        { model | pageState = Loaded (Player subModel) } ! [ Cmd.map PlayerMsg subCmd ]

                Ports.JSPlayerCurrTime currTime ->
                    let
                        ( subModel, subCmd ) =
                            Player.update
                                (Player.UpdatePlayerTime currTime)
                                playerModel
                    in
                        { model | pageState = Loaded (Player subModel) } ! [ Cmd.map PlayerMsg subCmd ]

        ( PortErr err, _ ) ->
            model ! []

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
        [ pageSubscriptions model
        , Ports.pullJSDataToElm PortMsg PortErr
        ]


pageSubscriptions : { a | pageState : PageState } -> Sub Msg
pageSubscriptions { pageState } =
    case pageState of
        Loaded (Player playerModel) ->
            let
                playerSub =
                    Player.subscriptions playerModel
            in
                Sub.map PlayerMsg playerSub

        _ ->
            Sub.none



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.pageState of
        Loading ->
            Views.frame <| Views.loading

        Loaded (Errored err) ->
            Views.frame <| Errored.view err

        Loaded Home ->
            Views.frame <|
                Home.view model.songs

        Loaded (Player playerModel) ->
            Views.frame <| Html.map PlayerMsg <| Player.view playerModel

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
                    if not model.homeLoaded || (Dict.isEmpty model.songs) then
                        ( Loading, Task.attempt HomeLoaded Home.load )
                    else
                        ( Loaded Home, Cmd.none )
            in
                { model | navOpen = False, homeLoaded = True, pageState = pageState }
                    ! [ cmd ]

        Just (Route.Player youTubeID) ->
            let
                maybeSong =
                    Dict.get (Types.youTubeIDtoString youTubeID) model.songs

                ( pageState, cmd ) =
                    case maybeSong of
                        Nothing ->
                            ( Loading
                            , Task.attempt (PlayerSongChordsLoaded youTubeID) <| Player.loadWithSong youTubeID
                            )

                        Just song ->
                            case song.chords of
                                Nothing ->
                                    ( Loaded (Player <| Player.init song)
                                    , Task.attempt (PlayerChordsLoaded song.youtube_id) <| Player.load song.youtube_id
                                    )

                                Just _ ->
                                    ( Loaded (Player <| Player.init song)
                                    , Cmd.none
                                    )
            in
                { model | navOpen = False, pageState = pageState }
                    ! [ cmd, Cmd.map PlayerMsg Player.getWindowSize ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
