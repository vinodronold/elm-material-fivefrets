module Route
    exposing
        ( Route(..)
        , fromLocation
        , href
        , modifyUrl
        )

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>))
import Types


type Route
    = Root
    | Home
    | Player Types.YouTubeID


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home (Url.s "")
        , Url.map Player (Url.s "player" </> Types.youTubeIDParser)
        ]


href : Route -> String
href page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                Player youTubeID ->
                    [ "player", Types.youTubeIDtoString youTubeID ]
    in
        "#/" ++ String.join "/" pieces


modifyUrl : Route -> Cmd msg
modifyUrl =
    href >> Navigation.modifyUrl


newUrl : Route -> Cmd msg
newUrl =
    href >> Navigation.newUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Root
    else
        Url.parseHash route location
