module Pages.News exposing (..)

import Layout.Grid as Grid
import Layout.Flex as Flex

import Model exposing (..)
import Styles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Date

newsCard : News -> Html Msg
newsCard {title, content, dateCreated} =
    let
        year  = toString <| Date.year dateCreated
        month = toString <| Date.month dateCreated
        day   = toString <| Date.day dateCreated

        niceDate = day ++ "." ++ month ++ "." ++ year
    in
        div [class "item-card"] 
        [
            h4 [class "has-text-center is-size-5"] [text title],
            p  [class "has-text-justify"] [text content],
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
    Flex.container Flex.Column model.isMobile [] 
        <| List.map (newsGroupView model) model.newsList