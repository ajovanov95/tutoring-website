module Model exposing(..)

import Bootstrap.Navbar as Navbar

import Window

import Regex

import Date

type Page = 
    PageProgramming | 
    PageAlgorithms  |
    PageMathematics |

    PageHome    |
    PageNews    |
    PageAboutUs |
    PageContact

type HeaderMsg =
    ProgrammingClicked |
    AlgorithmsClicked  |
    MathematicsClicked |

    HomeClicked    |
    NewsClicked    |
    AboutUsClicked |
    ContactClicked 

type Msg = Header HeaderMsg |
           NavbarMsg Navbar.State |
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

    isMobile: Bool,

    navbarState : Navbar.State 
}

initialModel : Model
initialModel = 
    let 
        fst (a, _) = a
    in {
        page = PageHome,

        -- newsList = [],

        newsList = [{title = "Hello", content="This be news", 
                    dateCreated = Date.fromTime 1234567891234},
                    {title = "Message", content="Something very important",
                    dateCreated = Date.fromTime 1254567861234}
                ],

        userAgent = "",

        windowSize = {width = 1366, height = 768},
        
        isMobile = False,

        navbarState = fst <| Navbar.initialState NavbarMsg
    }

userAgentCheckMobile : String -> Bool
userAgentCheckMobile ua = Regex.contains (Regex.regex "(M|m)obile") ua