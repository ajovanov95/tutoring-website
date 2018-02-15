module Pages.Home exposing (homePage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Flex

import Model exposing (..)

homePage : Model -> Html Msg
homePage model =
    Flex.container Flex.Column model.isMobile [] 
        [h1 [class "text-center display-4"] [text "Добредојдовте во светот на знаењето."],
         h1 [class "text-left"] [text "За нас"],
         p [] [text "Александар Јованов е главниот и единствениот учител тука. Треба ова секција да ја направи како card UI со сликичка."],
         h1 [class "text-left"] [text "Ценовник"]
        ]