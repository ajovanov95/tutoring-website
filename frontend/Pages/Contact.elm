module Pages.Contact exposing (contactPage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Model exposing (..)
import Styles exposing (..)

import Layout.Flex as Flex

emailForm : Model -> Html Msg
emailForm model = 
    div [Flex.flexBasis 50, class "item-card"]
        [
            h3 [class "is-size-3"] [text "Пишете ни порака"],

            div [class "field"]
            [
                label [for "myemail", class "label"] [text "Вашата адреса на електронска пошта"],

                input [class "input", type_ "email", id "myemail",
                       onInput EmailSubjectChanged,
                       class (if model.isAddrValid then "is-primary" else "is-danger")
                       ] []
            ],

            div [class "field"]
            [
                label [for "myarea", class "label"] [text "Вашата порака"],
                textarea [class "textarea is-primary", rows 8, id "myemail",
                          onInput EmailBodyChanged] []
            ],

            button [class "button is-primary", onClick SendEmail] [text "Испрати порака"]
        ]

iconStyle : Attribute Msg
iconStyle = style ["margin-left" :> "16px"]

infoBoxes : Model -> Html Msg
infoBoxes model = 
    let
        margin = "margin-top" :> "1.5em" |> style_
    in
    Flex.container Flex.Column model.isMobile 
        [
            Flex.flexBasis 50, 
            [
                if not model.isMobile then "padding-left" :> "1em" 
                else "padding-top" :> "1.5em",
                "justify-content" :>  "space-between" 
            ] |> style
        ]
        [
            Flex.item [class "item-card"] [
                h3 [class "is-size-3"] [text "Телефон"],
                text "076 648 258",
                br [] [],
                text "Бројот е активен и на ",
                span [class "fab fa-viber", "color" :> "#59267c" |> style_] []
            ],
            Flex.container Flex.Column model.isMobile 
            [class "item-card", margin] [
                h3 [class "is-size-3"] [text "Наше присуство на социјални мрежи."],
                Flex.container Flex.Row model.isMobile 
                ["justify-content" :> "center" |> style_] [
                    a [href "https://www.linkedin.com/in/acepwns/", iconStyle] 
                        [span [class "fab fa-linkedin"] []],
                    a [href "https://github.com/ajovanov95", iconStyle, 
                       "color" :> "black" |> style_] 
                       [span [class "fab fa-github"] []]
                ]
            ],
            Flex.container Flex.Column model.isMobile 
            [class "item-card", margin] [
                h3 [class "is-size-3"] [text "Работно време"],
                Flex.item [] [text "Секој работен ден од 8 - 18ч."],
                Flex.item [] [text "Сабота од 10 - 16ч."]
            ]
        ]

contactPage : Model -> Html Msg
contactPage model =
    Flex.container Flex.Column model.isMobile 
    [] 
    [
        Flex.container Flex.Auto model.isMobile 
        [] 
        [
            emailForm model,

            infoBoxes model
        ], 
        (case model.emailConfirmation of 
            Nothing -> br [] []
            Just s  -> 
                div [class "hero is-success", 
                     ("padding", "15px") |> style_,
                     ("margin-top", "20px") |> style_] 
                    [p [class "suptitle"] [text s]]
        ) 
    ]