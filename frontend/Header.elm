module Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Model exposing (..)

import Color

import Bootstrap.Navbar as Navbar

createHeaderNavbar : Model -> Html Msg
createHeaderNavbar model = 
    let
        mkItem content msg page icon =
            let
                htm = case icon of 
                    Just iconClass -> 
                        [span [class <| iconClass ++ " icon-wobble", 
                               style [("margin-right", "10px")]
                               ] [], text content]
                    Nothing   -> [text content] 
            in
                Navbar.itemLink [
                    style [("border-bottom", 
                      if model.page == page && 
                         not model.isMobile then "2px orange solid" else "none"),
                         ("color", "white")],
                    attribute "data-toggle" "collapse", 
                    attribute "data-target" ".navbar-collapse", 
                    onClick (Header msg)] htm
    in
        Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.collapseSmall
        |> Navbar.brand [ href "#", onClick (Header HomeClicked)] 
          [div [] [span [class "fas fa-book icon-wobble", 
                         style [("margin-right", "10px")]] [], text "Часови"]]
        |> Navbar.darkCustom Color.blue
        |> Navbar.items
            [ mkItem "Дома" HomeClicked PageHome (Just "fas fa-home")
            , mkItem "Програмирање" ProgrammingClicked PageProgramming (Just "fas fa-tasks")
            , mkItem "Вести" NewsClicked PageNews (Just "fas fa-newspaper")
            , mkItem "Контакт" ContactClicked PageContact (Just "fas fa-phone")
            ]
        |> Navbar.view model.navbarState