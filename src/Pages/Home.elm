module Pages.Home exposing (..)

import Html exposing (Html)
import Data.Songs as Data
import Task exposing (Task)
import Http
import Views


--- TASK ---


load : Task Http.Error Data.Songs
load =
    Data.getSongs
        |> Http.toTask


view : Data.Songs -> Html msg
view =
    Views.listSongs
