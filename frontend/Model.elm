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

decodeNews : D.Decoder (List (NewsGroup))
decodeNews = 
    let
        dateDecoder = 
            D.map (Result.withDefault (Date.fromTime 0) << Date.fromString) D.string

        newsDecoder =
            D.map3 News 
                (D.field "newsTitle" D.string)
                (D.field "newsContent" D.string)
                (D.field "newsDateCreated" dateDecoder)

        groupDecoder = 
            D.map3 NewsGroup 
                (D.field "year" D.int)
                (D.field "month" D.int)
                (D.field "news" <| D.list newsDecoder)
    in
        D.list groupDecoder


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

userAgentCheckMobile : String -> Bool
userAgentCheckMobile ua = 
    Regex.contains (Regex.regex "(M|m)obile") ua