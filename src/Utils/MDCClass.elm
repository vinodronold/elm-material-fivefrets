module Utils.MDCClass
    exposing
        ( brandClass
        , classList
        , button
        , buttonDensed
        , buttonUnElevated
        , buttonIcon
        , icons
        , elevationTransition
        , linearProgress
        , linearProgressBar
        , linearProgressIndeterminate
        , linearProgressBarInner
        , linearProgressBuffer
        , linearProgressBufferingDots
        , linearProgressPrimaryBar
        , linearProgressSecondaryBar
        , list
        , listItem
        , listAvatarList
        , listItemGraphic
        , listItemText
        , theme
        , themePrimary
        , themeSecondary
        , themeBackground
        , themeSurface
        , themePrimaryBG
        , themeSecondaryBG
        , themeTextPrimaryOnBackground
        , topAppBar
        , topAppBarFixed
        , topAppBarFixedScrolled
        , topAppBarFixedAdjust
        , topAppBarRow
        , topAppBarActionItem
        , topAppBarSection
        , topAppBarSectionAlignStart
        , topAppBarSectionAlignEnd
        , topAppBarTitle
        , typography
        , typographyHeader3
        , typographySubtitle1
        , typographySubtitle2
        , typographyBody1
        , typographyBody2
        , typographyCaption
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



---- BUTTONS ----


button : Class
button =
    MDCClass "mdc-button"


formButtonClass : String -> Class
formButtonClass =
    formClass button


buttonRaised : Class
buttonRaised =
    formButtonClass "--raised"


buttonUnElevated : Class
buttonUnElevated =
    formButtonClass "--unelevated"


buttonDensed : Class
buttonDensed =
    formButtonClass "--densed"


buttonIcon : Class
buttonIcon =
    formButtonClass "__icon"


icons : Class
icons =
    MDCClass "material-icons"



---- ELEVATION ----


elevation : Class
elevation =
    MDCClass "mdc-elevation"


formElevationClass : String -> Class
formElevationClass =
    formClass elevation


elevate : Int -> Class
elevate z =
    formElevationClass <| "-z" ++ toString z


elevationTransition : Class
elevationTransition =
    formElevationClass "-transition"



---- LINEAR PROGRESS ----


linearProgress : Class
linearProgress =
    MDCClass "mdc-linear-progress"


formlinearProgressClass : String -> Class
formlinearProgressClass =
    formClass linearProgress


linearProgressIndeterminate : Class
linearProgressIndeterminate =
    formlinearProgressClass "--indeterminate"


linearProgressBufferingDots : Class
linearProgressBufferingDots =
    formlinearProgressClass "__buffering-dots"


linearProgressBuffer : Class
linearProgressBuffer =
    formlinearProgressClass "__buffer"


linearProgressBar : Class
linearProgressBar =
    formlinearProgressClass "__bar"


linearProgressPrimaryBar : Class
linearProgressPrimaryBar =
    formlinearProgressClass "__primary-bar"


linearProgressSecondaryBar : Class
linearProgressSecondaryBar =
    formlinearProgressClass "__secondary-bar"


linearProgressBarInner : Class
linearProgressBarInner =
    formlinearProgressClass "__bar-inner"



---- LIST ----


list : Class
list =
    MDCClass "mdc-list"


formListClass : String -> Class
formListClass =
    formClass list


listItem : Class
listItem =
    formListClass "-item"


listAvatarList : Class
listAvatarList =
    formListClass "-avatar-list"


listItemGraphic : Class
listItemGraphic =
    formListClass "-item__graphic"


listItemText : Class
listItemText =
    formListClass "-item__text"



---- THEMES ----


theme : Class
theme =
    MDCClass "mdc-theme"


formThemeClass : String -> Class
formThemeClass =
    formClass theme


themePrimary : Class
themePrimary =
    formThemeClass "--primary"


themeSecondary : Class
themeSecondary =
    formThemeClass "--secondary"


themeBackground : Class
themeBackground =
    formThemeClass "--background"


themeSurface : Class
themeSurface =
    formThemeClass "--surface"


themePrimaryBG : Class
themePrimaryBG =
    formThemeClass "--primary-bg"


themeSecondaryBG : Class
themeSecondaryBG =
    formThemeClass "--secondary-bg"


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


topAppBarFixedScrolled : Class
topAppBarFixedScrolled =
    formTopAppBarClass "--fixed-scrolled"


topAppBarFixedAdjust : Class
topAppBarFixedAdjust =
    formTopAppBarClass "--fixed-adjust"


topAppBarRow : Class
topAppBarRow =
    formTopAppBarClass "__row"


topAppBarActionItem : Class
topAppBarActionItem =
    formTopAppBarClass "__action-item"


topAppBarSection : Class
topAppBarSection =
    formTopAppBarClass "__section"


topAppBarSectionAlignStart : Class
topAppBarSectionAlignStart =
    formTopAppBarClass "__section--align-start"


topAppBarSectionAlignEnd : Class
topAppBarSectionAlignEnd =
    formTopAppBarClass "__section--align-end"


topAppBarTitle : Class
topAppBarTitle =
    formTopAppBarClass "__title"



---- Typography ----


typography : Class
typography =
    MDCClass "mdc-typography"


formTypographyClass : String -> Class
formTypographyClass =
    formClass typography


typographyHeader3 : Class
typographyHeader3 =
    formTypographyClass "--headline6"


typographySubtitle1 : Class
typographySubtitle1 =
    formTypographyClass "--subtitle1"


typographySubtitle2 : Class
typographySubtitle2 =
    formTypographyClass "--subtitle2"


typographyBody1 : Class
typographyBody1 =
    formTypographyClass "--body1"


typographyBody2 : Class
typographyBody2 =
    formTypographyClass "--body2"


typographyCaption : Class
typographyCaption =
    formTypographyClass "--caption"
