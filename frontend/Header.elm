module Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Model exposing (..)

import Styles exposing (..)

makeIcon : String -> Bool -> Html Msg
makeIcon name isActive = 
    let
        styleActive  = ["color" :> "black"] |> style
    in
    span 
    ([class <| "fas " ++ name ++ " icon-wobble",
     "margin-right" :> "10px" |> style_] 
      ++ if isActive then [styleActive] else []) []

makeNavbarItem : String -> String -> Bool -> HeaderMsg -> Page -> Html Msg
makeNavbarItem content iconName isActive msg page =
    a [class "navbar-item", onClick <| Header msg] 
    [
        makeIcon iconName isActive,
        text content
    ]

createHeaderNavbar : Model -> Html Msg
createHeaderNavbar model =
    nav [class "navbar is-transparent is-warning"] 
    [
        -- BRAND and BURGER
        div [class "navbar-brand is-warning", "padding" :> "1.5em" |> style_] 
        [
            p [class "is-large"] [text "Часови"],

            div [class "navbar-burger burger", attribute "data-target" "navMenu"] 
            [
                -- empty spans actually are neccessary for stuff to work :/
                span [] [],
                span [] [],
                span [] []
            ]
        ],
        
        -- MENU
        div [class "navbar-menu", id "navMenu"] 
        [
            -- LEFT SIDE ITEMS
            div [class "navbar-start"] 
            [
                makeNavbarItem "Дома" "fa-home" 
                                (model.page == PageHome) HomeClicked PageHome,
                makeNavbarItem "Програмирање" "fa-tasks"
                                (model.page == PageProgramming) ProgrammingClicked PageProgramming,
                makeNavbarItem "Вести" "fa-newspaper" 
                                (model.page == PageNews) NewsClicked PageNews,
                makeNavbarItem "Контакт" "fa-phone" 
                                (model.page == PageContact) ContactClicked PageContact
            ]
        ]
    ]