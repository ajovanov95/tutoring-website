module Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Model exposing (..)

import Bootstrap.Navbar as Navbar

createHeaderNavbar : Model -> Html Msg
createHeaderNavbar model = 
    Navbar.config NavbarMsg
    |> Navbar.withAnimation
    |> Navbar.collapseMedium            -- Collapse menu at the medium breakpoint
    |> Navbar.brand [ href "#"] [ text "Часови"]
    |> Navbar.success
    |> Navbar.items
        [ Navbar.itemLink [href "#home", onClick (Header HomeClicked)] [ text "Дома"]
        , Navbar.itemLink [href "#programming", onClick (Header ProgrammingClicked)] [ text "Програмирање"]
        , Navbar.itemLink [href "#algorithms", onClick (Header AlgorithmsClicked)] [ text "Алгоритми"]
        , Navbar.itemLink [href "#mathematics", onClick (Header MathematicsClicked)] [ text "Математика"]
        , Navbar.itemLink [href "#news", onClick (Header NewsClicked)] [ text "Вести"]
        , Navbar.itemLink [href "#aboutus", onClick (Header AboutUsClicked)] [ text "За нас"]
        , Navbar.itemLink [href "#contact", onClick (Header ContactClicked)] [ text "Контакт"]
        ]
    |> Navbar.view model.navbarState