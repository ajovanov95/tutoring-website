module Admin exposing(..)

import Layout.Flex as Flex

import Model exposing (..)
import Api

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Date
import Time
import Task

import Date.Format exposing (format)

import Styles exposing (..)

insertNews : AdminModel -> Cmd AdminMsg
insertNews model =
    let 
        news = {
            title = model.newsTitle,
            content = model.newsContent,
            dateCreated = model.currentDate
        }
    in 
        case String.toInt model.token of 
            Ok token -> Api.insertNewsCmd news token 
            Err _    -> Cmd.none

update : AdminMsg -> AdminModel -> (AdminModel, Cmd AdminMsg)
update msg model =
    case msg of 
        TimeTicked -> (model, Task.perform DateChanged Date.now)
        DateChanged nd -> ({ model | currentDate = nd }, Cmd.none)

        TokenFieldChanged s -> ({model | token = s}, Cmd.none)
        NewsTitleChanged  s -> ({model | newsTitle = s}, Cmd.none)
        NewsContentChanged s -> ({model | newsContent = s}, Cmd.none)

        InsertNewsButtonClicked -> (model, insertNews model)
        RequestTokenButtonClicked -> (model, Api.requestTokenCmd)

        PostResponse s -> ({model | actionResponse = s}, Cmd.none)

subscriptions : AdminModel -> Sub AdminMsg
subscriptions model =
    Sub.batch [
        Time.every Time.second (\_ -> TimeTicked)
    ]

view : AdminModel -> Html AdminMsg
view model = 
    Flex.container Flex.Column False [Flex.h100] [
        -- header, bold and powerful
        Flex.item [class "hero is-primary", Flex.w100, 
                   "padding" :> "25px" |> style_,
                   "font-size" :> "25pt" |> style_
                   ] [
                       Flex.container Flex.Row False [Flex.w100] [
                           text "Hello Admin",
                           Flex.item [Flex.flexGrow 1] [], -- space
                           div [class "button is-danger is-large", 
                           onClick RequestTokenButtonClicked] [text "Request token"]
                       ]
                    ],
        -- Knowledge about the current date and token input textbox
        Flex.container Flex.Row False 
                   [class "hero", 
                   "align-items" :> "center" |> style_, 
                   "justify-content" :> "space-between" |> style_,
                   "background-color" :> "gray" |> style_,
                   "color" :> "white" |> style_,
                   "padding" :> "25px" |> style_,
                    "font-size" :> "18pt" |> style_] 
                    [
                        text <| format "%Y-%m-%d %H:%M:%S" model.currentDate,

                        input [type_ "text", class "input is-danger", 
                               placeholder "Insert your token here",
                               "margin-top" :> "15px" |> style_, 
                               "max-width" :> "60%" |> style_,
                               onInput TokenFieldChanged] [],

                        div [class "button is-medium is-success",
                            onClick InsertNewsButtonClicked
                            ] [text "Insert news"]
                    
                    ],
        -- Part of the website where to insert the news
        Flex.container Flex.Column False 
            ["padding" :> "15px" |> style_, 
            "align-items" :> "center" |> style_,
            "justify-content" :> "flex-start" |> style_,
            class "hero is-info", Flex.flexGrow 1] 
            [
                input [type_ "text", class "input", 
                       "font-size" :> "18pt" |> style_,
                       placeholder "News title", 
                       onInput NewsTitleChanged] [],
                textarea [class "textarea is-primary",
                          "margin-top" :> "15px" |> style_,
                          "font-size" :> "15pt" |> style_, 
                          placeholder "News contents",
                          onInput NewsContentChanged, rows 12] []
            ],
        Flex.item [class "hero is-warning", 
                            ["padding" :> "20px", "font-size" :> "20px"]
                            |> style,
                            Flex.w100] [
                    text (
                        case model.actionResponse of
                            "" -> "There is no server response. Request something."
                            s  -> s
                    )
                ]
    ]

main : Program Never AdminModel AdminMsg
main = Html.program {
        init = (initialAdminModel, Cmd.none),
        update = update,
        subscriptions = subscriptions,
        view = view
    }