port module Ports exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Types
import Time exposing (Time)


pushDataToJS : ElmDataOut -> Cmd msg
pushDataToJS data =
    case data of
        HomeLoaded ->
            elmData { tag = "HomeLoaded", data = Encode.null }

        PlayerLoaded playerID youTubeID ->
            elmData
                { tag = "PlayerLoaded"
                , data =
                    Encode.object
                        [ ( "youTubeID", Encode.string <| Types.youTubeIDtoString youTubeID )
                        , ( "playerID", Encode.string <| Types.ytPlayerIDToString playerID )
                        ]
                }

        SetPlayerState playerStatus ->
            case playerStatus of
                Types.Playing ->
                    elmData { tag = "PlayVideo", data = Encode.null }

                Types.Paused ->
                    elmData { tag = "PauseVideo", data = Encode.null }

                Types.Ended ->
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
    = HomeLoaded
    | PlayerLoaded Types.YTPlayerID Types.YouTubeID
    | SetPlayerState Types.PlayerStatus
    | GetPlayerCurrTime
    | SeekTo Time


port elmData : PortData -> Cmd msg


port jsData : (PortData -> msg) -> Sub msg
