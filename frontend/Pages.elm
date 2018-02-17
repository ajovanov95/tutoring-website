module Pages exposing (createPage)

import Model exposing (..)

import Html exposing (Html)

import Pages.Home as Home
import Pages.Programming as Programming
import Pages.News as News
import Pages.Contact as Contact

createPage : Model -> Html Msg
createPage model =
    case model.page of
        PageHome        -> Home.homePage model
        PageProgramming -> Programming.programmingPage model
        PageContact     -> Contact.contactPage model
        PageNews        -> News.newsPage model 

