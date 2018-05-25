module Views exposing (frame, loading, listSongs)

import Html exposing (Html, div, header, img, main_, text, section, span, ul, li)
import Html.Attributes exposing (attribute, src)
import Data.Songs as Data
import Utils.MDCClass as MDCClass


-- import Route

import Dict
import Types


topBar : Html msg
topBar =
    header
        [ MDCClass.classList
            [ MDCClass.topAppBar
            , MDCClass.topAppBarFixed
            , MDCClass.topAppBarFixedScrolled
            , MDCClass.themeTextPrimaryOnBackground
            , MDCClass.brandClass "header"
            ]
        ]
        [ div [ MDCClass.classList [ MDCClass.topAppBarRow, MDCClass.themeBackground ] ]
            [ section [ MDCClass.classList [ MDCClass.topAppBarSection ] ]
                [ span [ MDCClass.classList [ MDCClass.topAppBarTitle, MDCClass.brandClass "fivefrets" ] ]
                    [ text "fivefrets" ]
                ]
            ]
        ]


frame : Html msg -> Html msg
frame content =
    div [ MDCClass.classList [ MDCClass.typography, MDCClass.theme, MDCClass.brandClass "container" ] ]
        [ topBar
        , main_ [ MDCClass.classList [ MDCClass.topAppBarFixedAdjust, MDCClass.brandClass "content" ] ]
            [ content
            ]
        ]


loading : Html msg
loading =
    div [ MDCClass.classList [ MDCClass.brandClass "loader-container" ] ]
        [ div
            [ attribute "role" "progressbar"
            , MDCClass.classList [ MDCClass.linearProgress, MDCClass.linearProgressIndeterminate, MDCClass.brandClass "loader" ]
            ]
            [ div [ MDCClass.classList [ MDCClass.linearProgressBufferingDots ] ] []
            , div [ MDCClass.classList [ MDCClass.linearProgressBuffer ] ] []
            , div [ MDCClass.classList [ MDCClass.linearProgressBar, MDCClass.linearProgressPrimaryBar ] ]
                [ span [ MDCClass.classList [ MDCClass.linearProgressBarInner, MDCClass.themeSecondaryBG ] ] []
                ]
            , div [ MDCClass.classList [ MDCClass.linearProgressBar, MDCClass.linearProgressSecondaryBar ] ]
                [ span [ MDCClass.classList [ MDCClass.linearProgressBarInner ] ] []
                ]
            ]
        ]


listSongs : Data.Songs -> Html msg
listSongs songs =
    ul [ MDCClass.classList [ MDCClass.list, MDCClass.listAvatarList ] ] <|
        List.map displaySong <|
            Dict.values songs


displaySong : Data.Song -> Html msg
displaySong song =
    li [ MDCClass.classList [ MDCClass.listItem, MDCClass.elevationTransition, MDCClass.brandClass "song" ] ]
        [ img
            [ MDCClass.classList [ MDCClass.listItemGraphic, MDCClass.brandClass "youtube-image" ]
            , src <| Types.urlToString song.imgUrlDefault
            ]
            []
        , span [ MDCClass.classList [ MDCClass.listItemText ] ] [ text song.title ]
        ]
