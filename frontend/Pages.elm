module Pages exposing (pageProgramming, 
                       pageAlgorithms,
                       pageMathematics)

import Model exposing (..)
import Styles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, class, id)

newline : Html msg
newline = br [] []

pageProgramming : Model -> Html Msg
pageProgramming model = text "Програмирање"

pageAlgorithms : Model -> Html Msg
pageAlgorithms model = text "Алгоритми"

pageMathematics : Model -> Html Msg
pageMathematics model = text "Математика"