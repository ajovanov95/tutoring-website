module Model exposing(..)

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
           WindowResized Window.Size

type alias Model = {
    page: Page,
    
    userAgent: String,

    windowSize: Window.Size,

    isMobile: Bool 
}

initialModel : Model
initialModel = {
        page = PageProgramming,

        userAgent = "",

        windowSize = {width = 1366, height = 768},
        
        isMobile = False
    }

userAgentCheckMobile : String -> Bool
userAgentCheckMobile ua = Regex.contains (Regex.regex "(M|m)obile") ua