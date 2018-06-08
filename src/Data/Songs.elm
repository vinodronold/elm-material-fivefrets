module Data.Songs
    exposing
        ( Songs
        , Song
        , getSongs
        , getSongChords
        )

import Dict exposing (Dict)
import Data.ChordTime exposing (ChordTime)
import Types
import Config
import Http
import Json.Decode as Decode
import Data.ChordTime exposing (decodeChordTimeList)


type alias Songs =
    Dict ID Song


type alias ID =
    String


type alias Song =
    { youtube_id : Types.YouTubeID
    , title : String
    , imgUrlDefault : Types.URL
    , imgUrlMedium : Types.URL
    , chords : Maybe (List ChordTime)
    }



--- REQUESTS ---


getSongs : Http.Request Songs
getSongs =
    let
        requestUrl =
            (Types.urlToString Config.apiUrl) ++ "/songs"

        request =
            Http.get requestUrl decodeSongs
    in
        request


decodeSongs : Decode.Decoder Songs
decodeSongs =
    Decode.dict decodeSong


decodeSong : Decode.Decoder Song
decodeSong =
    Decode.map5 Song
        (Decode.at [ "youtube_id" ] Types.decodeYouTubeID)
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "imgUrlDefault" ] Types.decodeUrl)
        (Decode.at [ "imgUrlMedium" ] Types.decodeUrl)
        (Decode.succeed Nothing)


getSongChords : Types.YouTubeID -> Http.Request Song
getSongChords youTubeID =
    let
        requestUrl =
            (Types.urlToString Config.apiUrl) ++ "/song/chords/" ++ Types.youTubeIDtoString youTubeID

        request =
            Http.get requestUrl decodeSongChords
    in
        request


decodeSongChords : Decode.Decoder Song
decodeSongChords =
    Decode.map5 Song
        (Decode.at [ "youtube_id" ] Types.decodeYouTubeID)
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "imgUrlDefault" ] Types.decodeUrl)
        (Decode.at [ "imgUrlMedium" ] Types.decodeUrl)
        (Decode.at [ "chords" ] (Decode.maybe decodeChordTimeList))
