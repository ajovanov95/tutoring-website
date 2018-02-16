module Pages.Home exposing (homePage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Flex

import Model exposing (..)
import Styles exposing (..)

import Bootstrap.Table as Table

pricelistTable : Html Msg
pricelistTable = 
    let 
        mkRow service price = 
            Table.tr [] [
                  Table.td [] [text service],
                  Table.td [] [text price]
            ]
    in
        Table.table { 
            options = [],
            thead = Table.simpleThead [
                Table.th [] [text "Услуга"],
                Table.th [] [text "Цена"]
            ], 
            tbody = Table.tbody [] [
                mkRow "1 час од 45 минути по програмирање" "250 денари",
                mkRow "2 часa од 45 минути по програмирање" "450 денари",
                mkRow "3 часa од 45 минути по програмирање" "600 денари",
                mkRow "1 час од 45 минути по математика" "150 денари",
                mkRow "2 часa од 45 минути по математика" "250 денари",
                mkRow "3 часa од 45 минути по математика" "400 денари",
                mkRow "4 и повеќе часа" "По договор",
                mkRow "Проектни задачи" "По договор"
            ]
        }

welcomeMessage : Html Msg
welcomeMessage = p [] [
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
    Flex.container Flex.Auto model.isMobile 
    [cardStyle, style [("align-items", "center")]] 
    [
        img [src "images/aleksandar.jpg", class "picture-frame"] [],
        p [class "aboutme-text"] [text "Дипломиран инженер по информатика."]
    ]

homePage : Model -> Html Msg
homePage model =
    Flex.container Flex.Column model.isMobile 
        [] 
        [h1 [class "text-center display-4"] [text "Добредојдовте во светот на знаењето."],
         welcomeMessage,
         h4 [class "text-left"] [text "За нас"],
         aboutUsCard model
        --  h4 [class "text-left"] [text "Ценовник"],
        --  pricelistTable
        ]