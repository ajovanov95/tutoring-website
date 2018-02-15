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
        mkItem hash content msg page icon =
            let
                htm = case icon of 
                    Just iconClass -> 
                        [span [class iconClass, 
                               style [("margin-right", "10px")]
                               ] [], text content]
                    Nothing   -> [text content] 
            in
                Navbar.itemLink [
                    href <| "#" ++ hash, 
                    -- style [("color", "black")],
                    style [("border-bottom", 
                      if model.page == page && 
                         not model.isMobile then "3px white solid" else "0px")],
                    attribute "data-toggle" "collapse", 
                    attribute "data-target" ".navbar-collapse", 
                    onClick (Header msg)] htm
    in
        Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.collapseSmall
        |> Navbar.brand [ href "#", onClick (Header HomeClicked)] 
          [div [] [span [class "fas fa-book", 
                         style [("margin-right", "10px")]] [], text "Часови"]]
        |> Navbar.darkCustom Color.blue
        |> Navbar.items
            [ mkItem "home" "Дома" HomeClicked PageHome (Just "fas fa-home")
            , mkItem "programming" "Програмирање" ProgrammingClicked PageProgramming Nothing
            -- , mkItem "algorithms" "Алгоритми" AlgorithmsClicked  
            , mkItem "mathematics" "Математика" MathematicsClicked PageMathematics Nothing
            , mkItem "news" "Вести" NewsClicked PageNews (Just "fas fa-newspaper")
            -- , mkItem "abous" "За нас" AboutUsClicked
            , mkItem "contact" "Контакт" ContactClicked PageContact (Just "fas fa-phone")
            ]
        |> Navbar.view model.navbarState