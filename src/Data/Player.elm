module Data.Player exposing (..)


type PlayerStatus
    = NotLoaded
    | UnStarted
    | Ended
    | Playing
    | Paused
    | Buffering
    | Cued


toPlayerStatus : Int -> PlayerStatus
toPlayerStatus i =
    case i of
        (-1) ->
            UnStarted

        0 ->
            Ended

        1 ->
            Playing

        2 ->
            Paused

        3 ->
            Buffering

        5 ->
            Cued

        _ ->
            NotLoaded


type YTPlayerID
    = YTPlayerID String


ytPlayerIDToString : YTPlayerID -> String
ytPlayerIDToString (YTPlayerID s) =
    s
