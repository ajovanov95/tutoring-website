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
        mkItem hash content msg =
            Navbar.itemLink [
                href <| "#" ++ hash, 
                attribute "data-toggle" "collapse", 
                attribute "data-target" ".navbar-collapse", 
                onClick (Header msg)] [ text content ]
    in
        Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.collapseSmall
        |> Navbar.brand [ href "#"] [ text "Часови"]
        |> Navbar.lightCustom Color.green
        |> Navbar.items
            [ mkItem "home" "Дома" HomeClicked
            , mkItem "programming" "Програмирање" ProgrammingClicked
            , mkItem "algorithms" "Алгоритми" AlgorithmsClicked  
            , mkItem "mathematics" "Математика" MathematicsClicked
            , mkItem "news" "Вести" NewsClicked 
            -- , mkItem "abous" "За нас" AboutUsClicked
            , mkItem "contact" "Контакт" ContactClicked
            ]
        |> Navbar.view model.navbarState