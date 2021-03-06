module Model exposing(..)

import Window
import Regex
import Date
import Http

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
           EmailSubjectChanged String |
           EmailBodyChanged String |
           SendEmail |
           NewsArrived (Result Http.Error (List NewsGroup)) |
           EmailConfirmationArrived (Result Http.Error String)

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

checkEmailAddressForValidity : String -> Bool
checkEmailAddressForValidity addr =
    let 
        re = Regex.regex ".+@.+\\.com(\\.mk)?"
    in
        Regex.contains re addr

type alias Model = {
    page: Page,
    newsList: List NewsGroup,

    emailAddressTo: String,
    emailContent: String,
    emailConfirmation: Maybe String,
    isAddrValid: Bool,

    userAgent: String,
    windowSize: Window.Size,
    isMobile: Bool
}

type AdminMsg =  
    TimeTicked |
    DateChanged Date.Date |

    TokenFieldChanged String |
    NewsTitleChanged String |
    NewsContentChanged String |
    
    InsertNewsButtonClicked |
    RequestTokenButtonClicked |

    PostResponse String

type alias AdminModel = {
    token : String,
    newsTitle: String,
    newsContent: String,
    currentDate: Date.Date,
    actionResponse: String
}

initialModel : Model
initialModel =  {
        page = PageHome,
        newsList = [],

        emailAddressTo = "",
        emailContent = "",
        emailConfirmation = Nothing,
        isAddrValid = True,

        userAgent = "",
        windowSize = {width = 1366, height = 768},
        isMobile = True -- mobile first
    }

initialAdminModel : AdminModel
initialAdminModel = {
    token = "0",
    newsTitle = "",
    newsContent = "",
    currentDate = Date.fromTime 1,
    actionResponse = ""
    }

userAgentCheckMobile : String -> Bool
userAgentCheckMobile ua = 
    Regex.contains (Regex.regex "(M|m)obile") ua