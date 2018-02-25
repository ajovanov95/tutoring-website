module Pages.News exposing (..)

import Grid

import Model exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Date

newsCard : News -> Html Msg
newsCard {title, content, dateCreated} =
    let
        year  = toString <| Date.year dateCreated
        month = toString <| Date.month dateCreated
        day   = toString <| Date.day dateCreated

        niceDate = day ++ " / " ++ month ++ " / " ++ year
    in
        div [class "item-card"] 
        [
            h4 [class "text-center"] [text title],
            p  [class "text-center"] [text content],
            h6 [class "text-center"] [text niceDate]
        ]

newsPage : Model -> Html Msg 
newsPage model = 
    let
        sorted = List.sortBy (Date.toTime << .dateCreated) model.newsList
    in
        Grid.container (Grid.autoSpecification model.isMobile) 
        [] -- ATTRS
        (List.map newsCard sorted)