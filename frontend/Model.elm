module Model exposing(..)

import Bootstrap.Navbar as Navbar

import Window

import Regex

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

type alias Model = {
    page: Page,
    
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

        userAgent = "",

        windowSize = {width = 1366, height = 768},
        
        isMobile = False,

        navbarState = fst <| Navbar.initialState NavbarMsg
    }

userAgentCheckMobile : String -> Bool
userAgentCheckMobile ua = Regex.contains (Regex.regex "(M|m)obile") ua