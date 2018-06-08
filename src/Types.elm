module Types exposing (..)

import Json.Decode as Decode
import UrlParser as Url
import List.Extra exposing ((!!))


---------------------------------------------------------------------------------------------------


type YouTubeID
    = YouTubeID String


decodeYouTubeID : Decode.Decoder YouTubeID
decodeYouTubeID =
    Decode.map YouTubeID Decode.string


youTubeIDParser : Url.Parser (YouTubeID -> a) a
youTubeIDParser =
    Url.custom "SONG" (Ok << YouTubeID)


youTubeIDtoString : YouTubeID -> String
youTubeIDtoString (YouTubeID s) =
    s



---------------------------------------------------------------------------------------------------


type URL
    = URL String


urlToString : URL -> String
urlToString (URL s) =
    s


decodeUrl : Decode.Decoder URL
decodeUrl =
    Decode.map URL Decode.string



---------------------------------------------------------------------------------------------------


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



---------------------------------------------------------------------------------------------------


type YTPlayerID
    = YTPlayerID String


ytPlayerIDToString : YTPlayerID -> String
ytPlayerIDToString (YTPlayerID s) =
    s


ytPlayerID : YTPlayerID
ytPlayerID =
    YTPlayerID "YTPlayerID"



---------------------------------------------------------------------------------------------------


type Chord
    = Chord Note Quality
    | NoChord


chordName : Chord -> String
chordName chord =
    case chord of
        Chord note quality ->
            noteToString note ++ qualityToString quality

        NoChord ->
            "N"


type Note
    = A
    | As
    | Bf
    | B
    | C
    | Cs
    | Df
    | D
    | Ds
    | Ef
    | E
    | F
    | Fs
    | Gf
    | G
    | Gs
    | Af


noteToString : Note -> String
noteToString note =
    let
        noteStr =
            Basics.toString note
    in
        if noteStr |> String.endsWith "f" then
            (noteStr |> String.dropRight 1) ++ "b"
        else if noteStr |> String.endsWith "s" then
            (noteStr |> String.dropRight 1) ++ "#"
        else
            noteStr


notes : List Note
notes =
    [ A, Bf, B, C, Cs, D, Ds, E, F, Fs, G, Gs ]


noteToIndex : Note -> Int
noteToIndex note =
    case note of
        A ->
            0

        As ->
            1

        Bf ->
            1

        B ->
            2

        C ->
            3

        Cs ->
            4

        Df ->
            4

        D ->
            5

        Ds ->
            6

        Ef ->
            6

        E ->
            7

        F ->
            8

        Fs ->
            9

        Gf ->
            9

        G ->
            10

        Gs ->
            11

        Af ->
            11


type alias Transpose =
    Int


type alias Capo =
    Int


transformChord : Transpose -> Capo -> Chord -> Chord
transformChord xpose capo chord =
    case chord of
        NoChord ->
            NoChord

        Chord note quality ->
            let
                idx =
                    ((noteToIndex note) + (xpose - capo)) % 12

                transposedNote =
                    Maybe.withDefault note <| notes !! idx
            in
                Chord transposedNote quality



---------------------------------------------------------------------------------------------------


type Quality
    = Major
    | Minor


qualityToString : Quality -> String
qualityToString q =
    case q of
        Major ->
            ""

        Minor ->
            "m"



---------------------------------------------------------------------------------------------------
