module Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Model exposing (..)

import Styles exposing (..)

makeIcon : String -> Model -> Page -> Html Msg
makeIcon name model page = 
    let
        isActive     = model.page == page
        isMobile     = model.isMobile
        styleActive  = (if isMobile then ["color" :> "#FF9F00"] 
                                    else ["color" :> "white"]) |> style
    in
        span 
        ([class <| "fas " ++ name ++ " icon-wobble",
        "margin-right" :> "10px" |> style_] 
        ++ if isActive then [styleActive] else []) []

makeNavbarItem : String -> String -> Model -> HeaderMsg -> Page -> Html Msg
makeNavbarItem content iconName model msg page =
    a [class "navbar-item", onClick <| Header msg] 
    [
        makeIcon iconName model page,
        text content
    ]

createHeaderNavbar : Model -> Html Msg
createHeaderNavbar model =
    nav [class "navbar is-transparent navbar-additions"] 
    [
        -- BRAND and BURGER
        div [class "navbar-brand", 
             ["margin-left" :> "0.75em", "color" :> "black"] |> style] 
        [
            p [style ["font-size" :> "24pt"]] [text "Часови"],

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
                makeNavbarItem "Дома" "fa-home" model HomeClicked PageHome,
                makeNavbarItem "Програмирање" "fa-tasks" model ProgrammingClicked PageProgramming,
                makeNavbarItem "Вести" "fa-newspaper" model NewsClicked PageNews,
                makeNavbarItem "Контакт" "fa-phone" model ContactClicked PageContact
            ]
        ]
    ]