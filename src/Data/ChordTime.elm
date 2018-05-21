module Data.ChordTime exposing (ChordTime, getTime, getChords)

import Time exposing (Time)
import Types
import Config
import Http
import Json.Decode as Decode
import List.Extra exposing ((!!))


-----------------------------------------------------------------------


type alias ChordTime =
    ( Time, Types.Chord )


getTime : Maybe ChordTime -> Time
getTime chordTime =
    case chordTime of
        Nothing ->
            0

        Just ( t, _ ) ->
            t



-----------------------------------------------------------------------


getChords : Types.YouTubeID -> Http.Request (List ChordTime)
getChords youTubeID =
    let
        requestUrl =
            (Types.urlToString Config.apiUrl) ++ "/chords/" ++ Types.youTubeIDtoString youTubeID

        request =
            Http.get requestUrl decodeChordTimeList
    in
        request


decodeChordTimeList : Decode.Decoder (List ChordTime)
decodeChordTimeList =
    Decode.keyValuePairs Decode.int
        |> Decode.andThen decodeChordTime
        |> Decode.andThen reverseDecodeList


reverseDecodeList : List a -> Decode.Decoder (List a)
reverseDecodeList xs =
    xs |> List.reverse |> Decode.succeed


decodeChordTime : List ( String, Int ) -> Decode.Decoder (List ChordTime)
decodeChordTime raw =
    raw
        |> List.map rawToChordTime
        |> Decode.succeed


rawToChordTime : ( String, Int ) -> ChordTime
rawToChordTime ( s, i ) =
    (,)
        (Result.withDefault 0 (String.toFloat s))
        (defaultChords !! i |> Maybe.withDefault Types.NoChord)


defaultChords : List Types.Chord
defaultChords =
    [ Types.Chord Types.A Types.Minor
    , Types.Chord Types.Bf Types.Minor
    , Types.Chord Types.B Types.Minor
    , Types.Chord Types.C Types.Minor
    , Types.Chord Types.Cs Types.Minor
    , Types.Chord Types.D Types.Minor
    , Types.Chord Types.Ds Types.Minor
    , Types.Chord Types.E Types.Minor
    , Types.Chord Types.F Types.Minor
    , Types.Chord Types.Fs Types.Minor
    , Types.Chord Types.G Types.Minor
    , Types.Chord Types.Gs Types.Minor
    , Types.Chord Types.A Types.Major
    , Types.Chord Types.Bf Types.Major
    , Types.Chord Types.B Types.Major
    , Types.Chord Types.C Types.Major
    , Types.Chord Types.Cs Types.Major
    , Types.Chord Types.D Types.Major
    , Types.Chord Types.Ds Types.Major
    , Types.Chord Types.E Types.Major
    , Types.Chord Types.F Types.Major
    , Types.Chord Types.Fs Types.Major
    , Types.Chord Types.G Types.Major
    , Types.Chord Types.Gs Types.Major
    , Types.NoChord
    ]
