module Pages.Contact exposing (contactPage)

import Html exposing (..)
import Html.Attributes exposing (..)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Button as Button

import Model exposing (..)

contactPage : Model -> Html Msg
contactPage model =
    Form.form [] 
    [   h3 [] [text "Пишете ни електронска порака"],
        Form.group [] 
        [
            Form.label [for "myemail"] [text "Вашата адреса на електронска пошта"],
            Input.email [Input.id "myemail"],

            Form.label [for "myarea"] [text "Вашата порака"],
            Textarea.textarea [Textarea.id "myarea", Textarea.rows 7] ,

            br [] [],
            Button.button [Button.outlinePrimary, Button.block] [text "Испрати порака"]
        ],
        hr [] [],
        h3 [] [text "Побарајте не на мобилен телефон"],
        div [] [
            text "Можете да не добиете на 076 648 258. Бројот е активен и на ",
            span [class "fab fa-viber", style [("color", "#59267c")]] []
        ],
        hr [] [],
        h3 [] [text "Наше присуство на социјални мрежи."],
        -- text "Тука оди присуство.",
        a [href "https://www.linkedin.com/in/acepwns/"] [span [class "fab fa-linkedin"] []],
        hr [] [],
        h3 [] [text "Работно време"],
        text "Секој работен ден од 8-18ч. Сабота од 10-16ч. Недела не работиме."
    ]