module Pages.News exposing (..)

import Grid

import Model exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Card as Card

import Date

newsCard : News -> Html Msg
newsCard {title, content, dateCreated} =
    let
        year = toString <| Date.year dateCreated
        month = toString <| Date.month dateCreated
        day  = toString <| Date.day dateCreated

        niceDate = day ++ " / " ++ month ++ " / " ++ year
    in
        Card.config [] |>
        Card.block [] [
            Card.titleH3 [class "text-center"] [text title],
            Card.text [] [p [] [text content]],
            Card.text [] [text niceDate]
        ] |>
        Card.view

newsPage : Model -> Html Msg 
newsPage model = 
    let
        sorted = List.sortBy (Date.toTime << .dateCreated) model.newsList
    in
        Grid.container (Grid.autoSpecification model.isMobile) 
        [] -- ATTRS
        (List.map newsCard sorted)