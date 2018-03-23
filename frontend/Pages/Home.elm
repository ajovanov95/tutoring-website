module Pages.Home exposing (homePage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Layout.Flex as Flex
 
import Model exposing (..)
import Styles exposing (..)

pricelistTable : Html Msg
pricelistTable = 
    let 
        mkRow service price = 
            tr [] [
                  td [] [text service],
                  td [] [text price]
            ]
    in
        table [class "table is-striped is-fullwidth"] 
        [
            thead [] 
            [
                tr []
                [
                    th [] [text "Услуга"],
                    th [] [text "Цена"]
                ]
            ],
            tbody [] 
            [
                mkRow "1 час од 45 минути по програмирање" "300 денари",
                mkRow "2 часa од 45 минути по програмирање" "500 денари",
                mkRow "3 часa од 45 минути по програмирање" "800 денари",
                mkRow "4 и повеќе часа" "По договор",
                mkRow "Проектни задачи" "По договор"
            ]
        ]

welcomeMessage : Html Msg
welcomeMessage = p [class "hero is-info is-bold", 
                    ["padding" :> "1.2em", 
                     "text-indent" :> "1.5em"] |> style] [
           h3 [class "title has-text-right-mobile"] 
           [text "Добредојдовте во светот на знаењето."],
           br [] [],
           text """
        Се наоѓате на вистинското место за решавање на сите ваши проблеми со 
        сите форми на испити, колоквиуми и испрашувања. 
        Ние ќе ве научиме и обучиме да ги поминете сите тестови со високи оценки 
        и уште поважно да се создадите со знаење кое после ќе ви користи во вашиот живот.
        Веруваме да практичноста доаѓа на прво место за разбирање на теоријата и затоа
        предавањата и лекциите ги структурираме така да работите едноставно ќе ви кликнат
        уште после еден час. Доаѓаме на локација по ваш избор. Правиме проектни и семинарски
        задачи.
    """
    ]

aboutUsCard : Model -> Html Msg
aboutUsCard model = 
    Flex.container Flex.Column model.isMobile 
    [class "item-card", style [("align-items", "center")]] 
    [
        img [src "images/aleksandar.jpg", class "picture-frame"] [],
        p [class "aboutme-text"] 
        [text """
            Александар Јованов - Дипломиран инженер по информатика.
              """]
    ]

homePage : Model -> Html Msg
homePage model =
    Flex.container Flex.Column model.isMobile 
    [] 
    [ 
        welcomeMessage,
        
        h3 [class "is-size-3"] 
           [text "За нас"],
        aboutUsCard model,
        
        h3 [class "is-size-3 has-text-right-mobile", style [("margin-top", if model.isMobile then "20px" else "10px")]] 
            [text "Нашите ниски цени"],
        pricelistTable,
        
        p [class "hero is-warning", ["padding" :> "1.2em"] |> style] 
          [text """Во група важи 5% попуст по секој член за секој член
                  (на пример тројца е 15% попуст)."""]
    ]