module Pages.Contact exposing (contactPage)

import Html exposing (..)
import Html.Attributes exposing (..)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Button as Button

import Model exposing (..)
import Styles exposing (..)

import Flex

emailForm : Bool -> Html Msg
emailForm isMobile = 
    Form.form [Flex.flexBasis 50, class "item-card"] [ 
        h3 [] [text "Пишете ни електронска порака"],
        Form.group [] 
        [
            Form.label [for "myemail"] [text "Вашата адреса на електронска пошта"],
            Input.email [Input.id "myemail"],

            Form.label [for "myarea"] [text "Вашата порака"],
            Textarea.textarea [Textarea.id "myarea", Textarea.rows 12] ,

            br [] [],
            Button.button ([Button.outlinePrimary, Button.block]) [text "Испрати порака"]
        ]
    ]
    
infoBoxes : Model -> Html Msg
infoBoxes model = 
    let
        margin = "margin-top" :> "1.5em" |> style_
    in
    Flex.container Flex.Column model.isMobile 
        [
            Flex.flexBasis 50, 
            [
                "padding-left" :> "2em",
                "justify-content" :> if model.isMobile then "center" else "start"
            ] |> style
        ]
        [
            Flex.item [class "item-card"] [
                h3 [] [text "Побарајте не на мобилен телефон"],
                text "Можете да не добиете на 076 648 258.",
                br [] [],
                text "Бројот е активен и на ",
                span [class "fab fa-viber", "color" :> "#59267c" |> style_] []
            ],
            Flex.container Flex.Column model.isMobile 
            [class "item-card", margin] [
                h3 [] [text "Наше присуство на социјални мрежи."],
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
                h3 [] [text "Работно време"],
                Flex.item [] [text "Секој работен ден од 8 - 18ч."],
                Flex.item [] [text "Сабота од 10 - 16ч."],
                Flex.item [] [text "Недела не работиме."]
            ]
        ]

iconStyle : Attribute Msg
iconStyle = style ["margin-left" :> "16px"]

contactPage : Model -> Html Msg
contactPage model =
    Flex.container Flex.Auto model.isMobile 
    [] 
    [
        emailForm model.isMobile,

        infoBoxes model
    ]