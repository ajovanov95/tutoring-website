module Pages exposing (createPage)

import Model exposing (..)

import Html exposing (Html, div, text)

import Pages.Home as Home
import Pages.Contact as Contact

import Pages.Programming as Programming


createPage : Model -> Html Msg
createPage model =
    let pageHtml = 
            case model.page of
                PageHome        -> Home.homePage model

                PageProgramming -> Programming.programmingPage model
                
                PageContact     -> Contact.contactPage model

                _               -> div [] [text "Implement me"]
    in
        pageHtml

