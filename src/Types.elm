module Types exposing (..)

import Json.Decode as Decode
import UrlParser as Url


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
    case note of
        A ->
            "A"

        As ->
            "A#"

        Bf ->
            "Bb"

        B ->
            "B"

        C ->
            "C"

        Cs ->
            "C#"

        Df ->
            "Db"

        D ->
            "D"

        Ds ->
            "D#"

        Ef ->
            "Eb"

        E ->
            "E"

        F ->
            "F"

        Fs ->
            "F#"

        Gf ->
            "Gb"

        G ->
            "G"

        Gs ->
            "G#"

        Af ->
            "Ab"



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
