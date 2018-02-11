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
            Button.button [Button.success, Button.block] [text "Испрати порака"]
        ],
        hr [] [],
        h3 [] [text "Побарајте не на мобилен телефон"],
        text "Можете да не добиете на 076 648 258. Бројот е активен и на viber.",
        hr [] [],
        h3 [] [text "Наше присуство на социјални мрежи."],
        text "Тука оди присуство.",
        hr [] [],
        h3 [] [text "Работно време"],
        text "Тука оди работно време"
    ]