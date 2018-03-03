module Model exposing(..)

import Window
import Regex
import Date
import Http
import Json.Decode as D

type Page = 
    PageProgramming | 
    PageHome    |
    PageNews    |
    PageContact

type HeaderMsg =
    ProgrammingClicked |
    HomeClicked    |
    NewsClicked    |
    ContactClicked 

type Msg = Header HeaderMsg |
           WindowResized Window.Size |
           NewsArrived (Result Http.Error (List NewsGroup))

type alias News = {
    title: String,
    content: String,
    dateCreated: Date.Date
}

type alias NewsGroup = {
    year : Int,
    month : Int,
    news : List News
}

decodeNews : D.Decoder (List (NewsGroup))
decodeNews = 
    let
        dateDecoder = D.map Date.fromTime D.float

        newsDecoder =
            D.map3 News 
                (D.field "title" D.string)
                (D.field "content" D.string)
                (D.field "dateCreated" dateDecoder)

        groupDecoder = 
            D.map3 NewsGroup 
                (D.field "year" D.int)
                (D.field "month" D.int)
                (D.field "news" <| D.list newsDecoder)
    in
        D.list groupDecoder

type alias Model = {
    page: Page,
    
    newsList: List NewsGroup,

    userAgent: String,

    windowSize: Window.Size,

    isMobile: Bool
}

initialModel : Model
initialModel =  {
        page = PageHome,

        newsList = [],

        userAgent = "",

        windowSize = {width = 1366, height = 768},
        
        isMobile = True -- mobile first
    }

userAgentCheckMobile : String -> Bool
userAgentCheckMobile ua = 
    Regex.contains (Regex.regex "(M|m)obile") ua