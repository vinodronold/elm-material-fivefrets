module Pages.Player exposing (..)

import AnimationFrame
import Time exposing (Time)
import Task exposing (Task)
import Http
import Html exposing (Html, button, div, text, fieldset, legend, span, i, img)
import Html.Attributes exposing (disabled, id, style, src, alt)
import Html.Events exposing (onClick)
import Window
import Dom
import Dom.Scroll as Scroll
import Types
import Ports
import Data.ChordTime as ChordTime exposing (ChordTime)
import Data.Songs as Songs exposing (Song)
import Device exposing (Device)
import Utils.MDCClass as MDCClass


type alias Model =
    { youtube_id : Types.YouTubeID
    , title : String
    , imgUrlMedium : Types.URL
    , playerID : Types.YTPlayerID
    , playerStatus : Types.PlayerStatus
    , playerTime : Maybe Time
    , playedChords : List ChordTime
    , currChord : Maybe ChordTime
    , nextChords : List ChordTime
    , transpose : Types.Transpose
    , capo : Types.Capo
    , device : Device
    , refreshFrame : Bool
    }


init : Song -> Model
init song =
    { youtube_id = song.youtube_id
    , title = song.title
    , imgUrlMedium = song.imgUrlMedium
    , playerID = Types.ytPlayerID
    , playerStatus = Types.NotLoaded
    , playerTime = Nothing
    , playedChords = []
    , currChord = Nothing
    , nextChords = Maybe.withDefault [] song.chords
    , transpose = 0
    , capo = 0
    , device = Device.classifyDevice <| Window.Size 0 0
    , refreshFrame = True
    }


load : Types.YouTubeID -> Task Http.Error (List ChordTime)
load youTubeID =
    ChordTime.getChords youTubeID
        |> Http.toTask


loadWithSong : Types.YouTubeID -> Task Http.Error Song
loadWithSong youTubeID =
    Songs.getSongChords youTubeID
        |> Http.toTask


type Msg
    = Tick Time
    | WindowResize Window.Size
    | UpdatePlayerStatus Types.PlayerStatus
    | UpdatePlayerTime Time
    | SeekToPosition Time
    | ControlCommand Controls
    | ScrollingToY


type Controls
    = ChangePlayerStatus Types.PlayerStatus
    | Transpose Step
    | Capo Step


type Step
    = Inc
    | Dec


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            -- >> Refresh on Alternate Browser Animation Frame
            if model.refreshFrame then
                { model | refreshFrame = not model.refreshFrame } ! [ Ports.pushDataToJS Ports.GetPlayerCurrTime ]
            else
                { model | refreshFrame = not model.refreshFrame } ! []

        WindowResize size ->
            { model | device = Device.classifyDevice size } ! []

        UpdatePlayerStatus playerStatus ->
            { model | playerStatus = playerStatus } ! []

        UpdatePlayerTime playerTime ->
            if
                (playerTime > ChordTime.getTime (List.head model.nextChords))
                    && (playerTime > ChordTime.getTime model.currChord)
            then
                let
                    getplayed =
                        case model.currChord of
                            Nothing ->
                                []

                            Just chord ->
                                model.playedChords ++ chord :: []
                in
                    case model.nextChords of
                        x :: xs ->
                            { model | playerTime = Just playerTime, playedChords = getplayed, currChord = Just x, nextChords = xs }
                                ! [ scrolling (List.length model.playedChords + 1) (getBlocks model.device) ]

                        [] ->
                            -- Chords ENDED
                            { model | playerTime = Just playerTime, playedChords = [], currChord = Nothing, nextChords = getplayed }
                                ! [ Ports.pushDataToJS <| Ports.SetPlayerState Types.Ended, scrollToTop diplayChordDomID ]
            else
                { model | playerTime = Just playerTime }
                    ! []

        SeekToPosition seekToTime ->
            let
                ( before, after ) =
                    getAllChords model
                        |> List.partition
                            (\( t, _ ) -> t < seekToTime)

                ( curr, next ) =
                    case after of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, [] )
            in
                { model
                    | playedChords = before
                    , currChord = curr
                    , nextChords = next
                }
                    ! [ Ports.pushDataToJS <| Ports.SeekTo seekToTime
                      , scrollToY (getScrollYPos (List.length before) (getBlocks model.device)) diplayChordDomID
                      ]

        ControlCommand control ->
            case control of
                ChangePlayerStatus playerStatus ->
                    if playerStatus == Types.Ended then
                        { model
                            | playedChords = []
                            , currChord = Nothing
                            , nextChords = getAllChords model
                        }
                            ! [ Ports.pushDataToJS <| Ports.SetPlayerState playerStatus
                              , scrollToTop diplayChordDomID
                              ]
                    else
                        model ! [ Ports.pushDataToJS <| Ports.SetPlayerState playerStatus ]

                Transpose step ->
                    case step of
                        Inc ->
                            { model | transpose = model.transpose + 1 } ! []

                        Dec ->
                            { model | transpose = model.transpose - 1 } ! []

                Capo step ->
                    case step of
                        Inc ->
                            { model | capo = model.capo + 1 } ! []

                        Dec ->
                            { model | capo = model.capo - 1 } ! []

        ScrollingToY ->
            model ! []


scrolling : Int -> Int -> Cmd Msg
scrolling totalChordsPlayed blocks =
    if totalChordsPlayed > blocks then
        let
            yPos =
                ((totalChordsPlayed // blocks) - 1) * 52
        in
            scrollToY (toFloat yPos) diplayChordDomID
    else
        Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowResize
        , playerSubscription model.playerStatus
        ]


playerSubscription : Types.PlayerStatus -> Sub Msg
playerSubscription playerStatus =
    if playerStatus == Types.Playing then
        AnimationFrame.times Tick
        -- Time.every (Time.second * 0.1) Tick
    else
        Sub.none


getWindowSize : Cmd Msg
getWindowSize =
    Task.perform WindowResize Window.size


getAllChords :
    { r
        | currChord : Maybe ChordTime
        , nextChords : List ChordTime
        , playedChords : List ChordTime
    }
    -> List ChordTime
getAllChords { playedChords, currChord, nextChords } =
    case currChord of
        Nothing ->
            nextChords

        Just curr ->
            playedChords ++ (curr :: nextChords)


view : Model -> Html Msg
view model =
    let
        displayNextChords =
            (List.indexedMap (displayChord model.transpose model.capo False) model.nextChords)

        displayCurrChord =
            case model.currChord of
                Nothing ->
                    []

                Just currChord ->
                    (displayChord model.transpose model.capo True 0 currChord) :: []

        displayPlayedChords =
            (List.map (displayChord model.transpose model.capo False 0) model.playedChords)
    in
        div [ MDCClass.classList [ MDCClass.brandClass "player player-youtube-clearfix" ] ]
            [ div [ MDCClass.classList [ MDCClass.typographyHeader3 ] ] [ text model.title ]
            , div [ MDCClass.classList [ MDCClass.brandClass "chords" ], id diplayChordDomID ]
                (displayPlayedChords ++ displayCurrChord ++ displayNextChords)
            , playerControls model
            , displayYouTubeVideo model
            ]


type alias ActiveChord =
    Bool


displayChord : Types.Transpose -> Types.Capo -> ActiveChord -> Int -> ChordTime -> Html Msg
displayChord xpose capo active idx ( time, chord ) =
    let
        chordName =
            chord
                |> Types.transformChord xpose capo
                |> Types.chordName
    in
        if active then
            div [ MDCClass.classList [ MDCClass.brandClass "chord chord-active" ] ] [ text chordName ]
        else
            div
                [ style [ ( "animationDelay", (toString <| getDelay idx) ++ "ms" ) ]
                , MDCClass.classList [ MDCClass.brandClass "chord" ]
                , onClick <| SeekToPosition time
                ]
                [ text chordName ]


getDelay : Int -> Int
getDelay idx =
    let
        unit =
            50

        max =
            20
    in
        if idx <= max then
            idx * unit
        else
            max * unit


playerControls : Model -> Html Msg
playerControls model =
    div [ MDCClass.classList [ MDCClass.brandClass "player-controls" ] ]
        [ displayPlayStopControl model
        , fieldset []
            [ legend [] [ text "Capo" ]
            , button
                [ MDCClass.classList [ MDCClass.button, MDCClass.buttonDensed, MDCClass.brandClass "ripple" ]
                , disabled <| (model.capo <= 0)
                , onClick <| ControlCommand <| Capo Dec
                ]
                [ i [ MDCClass.classList [ MDCClass.icons ] ] [ text "remove" ] ]
            , span [ MDCClass.classList [ MDCClass.brandClass "player-control-item" ] ] [ text <| toString model.capo ]
            , button
                [ MDCClass.classList [ MDCClass.button, MDCClass.buttonDensed, MDCClass.brandClass "ripple" ]
                , onClick <| ControlCommand <| Capo Inc
                ]
                [ i [ MDCClass.classList [ MDCClass.icons ] ] [ text "add" ] ]
            ]
        , fieldset []
            [ legend [] [ text "Transpose" ]
            , button
                [ MDCClass.classList [ MDCClass.button, MDCClass.buttonDensed, MDCClass.brandClass "ripple" ]
                , onClick <| ControlCommand <| Transpose Dec
                ]
                [ i [ MDCClass.classList [ MDCClass.icons ] ] [ text "remove" ] ]
            , span [ MDCClass.classList [ MDCClass.brandClass "player-control-item" ] ] [ text <| toString model.transpose ]
            , button
                [ MDCClass.classList [ MDCClass.button, MDCClass.buttonDensed, MDCClass.brandClass "ripple" ]
                , onClick <| ControlCommand <| Transpose Inc
                ]
                [ i [ MDCClass.classList [ MDCClass.icons ] ] [ text "add" ] ]
            ]
        ]


displayPlayStopControl : Model -> Html Msg
displayPlayStopControl model =
    let
        label =
            case model.playerStatus of
                Types.Playing ->
                    "Pause"

                _ ->
                    "Play"
    in
        if model.device.phone then
            div [] []
        else
            fieldset []
                [ legend [] [ text "Controls" ]
                , button
                    [ MDCClass.classList [ MDCClass.button, MDCClass.buttonDensed, MDCClass.brandClass "ripple" ]
                    , onClick <| playPauseMsg model.playerStatus
                    ]
                    [ i [ MDCClass.classList [ MDCClass.icons, MDCClass.buttonIcon ] ] [ text <| playPauseIcon model.playerStatus ]
                    , text label
                    ]
                , button
                    [ MDCClass.classList [ MDCClass.button, MDCClass.buttonDensed, MDCClass.brandClass "ripple" ]
                    , onClick (ControlCommand <| ChangePlayerStatus Types.Ended)
                    , disabled <| not ((model.playerStatus == Types.Playing) || (model.playerStatus == Types.Paused))
                    ]
                    [ i [ MDCClass.classList [ MDCClass.icons, MDCClass.buttonIcon ] ] [ text "stop" ]
                    , text "stop"
                    ]
                ]


displayYouTubeVideo : Model -> Html Msg
displayYouTubeVideo model =
    div
        [ MDCClass.classList [ MDCClass.brandClass "player-youtube" ]
        ]
        [ div
            [ id <| Types.ytPlayerIDToString model.playerID
            , MDCClass.classList [ MDCClass.brandClass "player-youtube-video" ]
            ]
            [ displayYouTubeImage model.imgUrlMedium ]
        , div [ MDCClass.classList [ MDCClass.brandClass "player-youtube-controls" ] ]
            [ button
                [ MDCClass.classList [ MDCClass.button, MDCClass.buttonUnElevated, MDCClass.brandClass "ripple" ]
                , onClick <| playPauseMsg model.playerStatus
                ]
                [ i [ MDCClass.classList [ MDCClass.icons ] ] [ text <| playPauseIcon model.playerStatus ]
                ]
            , button
                [ MDCClass.classList [ MDCClass.button, MDCClass.buttonUnElevated, MDCClass.brandClass "ripple" ]
                , onClick (ControlCommand <| ChangePlayerStatus Types.Ended)
                , disabled <| not ((model.playerStatus == Types.Playing) || (model.playerStatus == Types.Paused))
                ]
                [ i [ MDCClass.classList [ MDCClass.icons ] ] [ text "stop" ]
                ]
            ]
        ]


playPauseIcon : Types.PlayerStatus -> String
playPauseIcon playerStatus =
    case playerStatus of
        Types.Playing ->
            "pause"

        _ ->
            "play_arrow"


playPauseMsg : Types.PlayerStatus -> Msg
playPauseMsg playerStatus =
    case playerStatus of
        Types.Playing ->
            ControlCommand <| ChangePlayerStatus Types.Paused

        _ ->
            ControlCommand <| ChangePlayerStatus Types.Playing


displayYouTubeImage : Types.URL -> Html msg
displayYouTubeImage imgUrlMedium =
    img
        [ MDCClass.classList [ MDCClass.brandClass "player-image" ]
        , src <| Types.urlToString imgUrlMedium
        , alt "YouTube"
        ]
        []



---------------------------------------------------------------------------------------------------
-- UTILITIES
---------------------------------------------------------------------------------------------------


diplayChordDomID : Dom.Id
diplayChordDomID =
    "diplayChordID"


getBlocks : Device -> Int
getBlocks { tablet, phone } =
    if phone then
        4
    else if tablet then
        8
    else
        16



---> SCROLLING


getScrollYPos : Int -> Int -> Float
getScrollYPos idx blocks =
    toFloat ((idx // blocks) - 1) * 52


scrollToTop : Dom.Id -> Cmd Msg
scrollToTop =
    scrollToY 0


scrollToY : Float -> Dom.Id -> Cmd Msg
scrollToY pos domID =
    Scroll.toY domID pos
        |> Task.attempt (always ScrollingToY)



---------------------------------------------------------------------------------------------------
