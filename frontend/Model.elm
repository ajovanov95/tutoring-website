module Model exposing(..)

import Window

import Regex

import Date

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
           WindowResized Window.Size

type alias News = {
    title: String,
    content: String,
    dateCreated: Date.Date
}

type alias Model = {
    page: Page,
    
    newsList: List News,

    userAgent: String,

    windowSize: Window.Size,

    isMobile: Bool
}

initialModel : Model
initialModel =  {
        page = PageHome,

        -- newsList = [],

        newsList = [{title = "Hello", content="This be news", 
                    dateCreated = Date.fromTime 1234567891234},
                    {title = "Message", content="Something very important",
                    dateCreated = Date.fromTime 1254567861234}
                ],

        userAgent = "",

        windowSize = {width = 1366, height = 768},
        
        isMobile = True -- mobile first
    }

userAgentCheckMobile : String -> Bool
userAgentCheckMobile ua = 
    Regex.contains (Regex.regex "(M|m)obile") ua