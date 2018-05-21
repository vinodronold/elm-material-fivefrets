port module Ports exposing (..)

import Data.Player as Player
import Data.Songs as Song
import Json.Decode as Decode
import Json.Encode as Encode
import Types
import Time exposing (Time)


pushDataToJS : ElmDataOut -> Cmd msg
pushDataToJS data =
    case data of
        LoadYouTubeVideo playerID youTubeID ->
            elmData
                { tag = "LoadYouTubeVideo"
                , data =
                    Encode.object
                        [ ( "youTubeID", Encode.string <| Types.youTubeIDtoString youTubeID )
                        , ( "playerID", Encode.string <| Player.ytPlayerIDToString playerID )
                        ]
                }

        SetPlayerState playerStatus ->
            case playerStatus of
                Player.Playing ->
                    elmData { tag = "PlayVideo", data = Encode.null }

                Player.Paused ->
                    elmData { tag = "PauseVideo", data = Encode.null }

                Player.Ended ->
                    elmData { tag = "StopVideo", data = Encode.null }

                _ ->
                    elmData { tag = "SetPlayerState_NoOp", data = Encode.null }

        GetPlayerCurrTime ->
            elmData { tag = "GetPlayerCurrTime", data = Encode.null }

        SeekTo time ->
            elmData { tag = "SeekTo", data = Encode.float time }


pullJSDataToElm : (JSDataIn -> msg) -> (String -> msg) -> Sub msg
pullJSDataToElm tagger onError =
    jsData <| parseJSData tagger onError


parseJSData : (JSDataIn -> msg) -> (String -> msg) -> PortData -> msg
parseJSData tagger onError js =
    case js.tag of
        "JSPlayerStatus" ->
            case Decode.decodeValue Decode.int js.data of
                Ok jsPlayerStatus ->
                    tagger <| JSPlayerStatus jsPlayerStatus

                Err e ->
                    onError e

        "JSPlayerCurrTime" ->
            case Decode.decodeValue Decode.float js.data of
                Ok currTime ->
                    tagger <| JSPlayerCurrTime currTime

                Err e ->
                    onError e

        _ ->
            onError <| "Unexpected info from outside: " ++ toString jsData


type alias PortData =
    { tag : String
    , data : Encode.Value
    }


type JSDataIn
    = JSPlayerStatus Int
    | JSPlayerCurrTime Time


type ElmDataOut
    = LoadYouTubeVideo Player.YTPlayerID Types.YouTubeID
    | SetPlayerState Player.PlayerStatus
    | GetPlayerCurrTime
    | SeekTo Time


port elmData : PortData -> Cmd msg


port jsData : (PortData -> msg) -> Sub msg
