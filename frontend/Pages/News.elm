module Pages.News exposing (..)

import Layout.Grid as Grid
import Layout.Flex as Flex

import Model exposing (..)
import Styles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Date

import Markdown

newsCard : News -> Html Msg
newsCard {title, content, dateCreated} =
    let
        year  = toString <| Date.year dateCreated
        month = toString <| Date.month dateCreated
        day   = toString <| Date.day dateCreated

        niceDate = day ++ ". " ++ month ++ ". " ++ year
    in
        div [class "item-card"] 
        [
            h4 [class "has-text-center is-size-5"] [text title],
            Markdown.toHtml [class "has-text-justify"] content,
            h6 [class "has-text-center is-size-6"] [text niceDate]
        ]

newsGroupView : Model -> NewsGroup -> Html Msg --(Year, Month, List (News))
newsGroupView model grp =
    let
        hdr_ = toString grp.year ++ " - " ++ toString grp.month |> text
        hdr  = Flex.container Flex.Row False
          [["justify-content" :> "center", "align-items" :> "center"] |> style] [
                    h3 [class "has-text-left is-size-3"] [hdr_],
                    div [Flex.flexGrow 1, 
                            ["height" :> "3px", 
                            "background" :> "hsl(171, 100%, 41%)",
                            "margin-left" :> "5px"] |> style] []
            ]
    in
        div [] [
            hdr,
             Grid.container (Grid.autoSpecification model.isMobile) 
             [] -- ATTRS
             (List.map newsCard grp.news)
        ]

newsPage : Model -> Html Msg 
newsPage model = 
    if (List.length model.newsList) > 0 then
        Flex.container Flex.Column model.isMobile [] 
            <| List.map (newsGroupView model) model.newsList
    else
        Flex.container Flex.Column model.isMobile
        [["align-items" :> "center", "justify-content" :> "center"] |> style, 
         Flex.h100, class "is-size-2"]
        [text "Сеуште нема вести. Александар е веројатно слободен."]