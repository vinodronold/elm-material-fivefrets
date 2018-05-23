module Utils.MDCClass
    exposing
        ( brandClass
        , classList
        , theme
        , themeBackground
        , themeTextPrimaryOnBackground
        , topAppBar
        , topAppBarFixed
        , topAppBarFixedAdjust
        , topAppBarRow
        , topAppBarSection
        , topAppBarTitle
        , typography
        )

import Html
import Html.Attributes exposing (class)


type Class
    = MDCClass String
    | BrandClass String


type alias Classes =
    List Class


brandClass : String -> Class
brandClass s =
    BrandClass s


classList : Classes -> Html.Attribute msg
classList classes =
    classes
        |> List.map mdcClassToString
        |> String.join " "
        |> class


mdcClassToString : Class -> String
mdcClassToString cls =
    case cls of
        MDCClass s ->
            s

        BrandClass s ->
            s


formClass : Class -> String -> Class
formClass cls str =
    case cls of
        MDCClass s ->
            MDCClass <| s ++ str

        BrandClass s ->
            BrandClass <| s ++ str



---- THEMES ----


theme : Class
theme =
    MDCClass "mdc-theme"


formThemeClass : String -> Class
formThemeClass =
    formClass theme


themeBackground : Class
themeBackground =
    formThemeClass "--background"


themeTextPrimaryOnBackground : Class
themeTextPrimaryOnBackground =
    formThemeClass "--text-primary-on-background"



---- TOP APP BAR ----


topAppBar : Class
topAppBar =
    MDCClass "mdc-top-app-bar"


formTopAppBarClass : String -> Class
formTopAppBarClass =
    formClass topAppBar


topAppBarFixed : Class
topAppBarFixed =
    formTopAppBarClass "--fixed"


topAppBarFixedAdjust : Class
topAppBarFixedAdjust =
    formTopAppBarClass "--fixed-adjust"


topAppBarRow : Class
topAppBarRow =
    formTopAppBarClass "__row"


topAppBarSection : Class
topAppBarSection =
    formTopAppBarClass "__section"


topAppBarTitle : Class
topAppBarTitle =
    formTopAppBarClass "__title"



---- Typography ----


typography : Class
typography =
    MDCClass "mdc-typography"
