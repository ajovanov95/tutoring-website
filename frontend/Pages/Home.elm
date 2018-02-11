module Pages.Home exposing (homePage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Flex

import Model exposing (..)

homePage : Model -> Html Msg
homePage model =
    Flex.container Flex.Column model.isMobile [] 
        [h1 [class "text-center"] [text "Добредојдовте во светот на знаењето."],
         br [] [],
         p [] [text """
            Нашите стручни и посветени кадри ќе ви го разјаснат патот во маглината на програмирањето, алгоритмите и математика. 
         """],
         p [] [text """
            Ние ќе ве научиме тоа што сакате да го научите и многу повеќе бидејќи нашиот пристап е квалитет на прво место.
            """],
        p [] [text """Нашите цени се пристапни со големи намалувања за редовни клиенти."""],
        p [] [text "Почнете со дообразување со нас и приметете ја трансформацијата на вашето разбирање и вашите оценки."],
        h1 [class "text-center"] [text "За нас"],
        p [] [text "Александар Јованов е главниот и единствениот учител тука. Треба ова секција да ја направи како card UI со сликичка."],
        h1 [class "text-center"] [text "Ценовник"]
        ]