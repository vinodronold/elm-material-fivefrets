module Pages.Errored exposing (PageLoadError, pageLoadError, pageLoadErrorToString, view)

import Html exposing (Html, div, text)
import Utils.MDCClass as MDCClass


type PageLoadError
    = PageLoadError String


pageLoadError : String -> PageLoadError
pageLoadError =
    PageLoadError


pageLoadErrorToString : PageLoadError -> String
pageLoadErrorToString (PageLoadError str) =
    str


view : PageLoadError -> Html msg
view (PageLoadError errMessage) =
    div [ MDCClass.classList [ MDCClass.brandClass "error" ] ]
        [ div [ MDCClass.classList [ MDCClass.typographySubtitle1 ] ]
            [ text "☹ Sorry, Error in loading page." ]
        , div [ MDCClass.classList [ MDCClass.typographyCaption ] ]
            [ text errMessage ]
        ]
