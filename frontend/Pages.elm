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
pageProgramming model = div [] []

pageAlgorithms : Model -> Html Msg
pageAlgorithms model = div [] []

pageMathematics : Model -> Html Msg
pageMathematics model = div [] []