import Html exposing (..)

import Window
import Debug
import Task

import Layout.Flex as Flex

import Styles exposing (..)
import Model exposing (..)
import Pages exposing (..)
import Header

import Api as Api

-- INIT

type alias Flags = {
    userAgent: String
}

initialCmd : Cmd Msg
initialCmd =
    let 
        getWindowSizeCmd = Task.perform WindowResized Window.size
    in
        Cmd.batch [getWindowSizeCmd, Api.getNewsCmd]

initialization : Flags -> (Model, Cmd Msg)
initialization flags = 
    ({initialModel |
        userAgent = flags.userAgent, 
        isMobile  = userAgentCheckMobile flags.userAgent
      }, initialCmd)

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatePage p = { model | page = p }
        newModel =
            case msg of 
                WindowResized newSize -> { model | windowSize = newSize}
                Header hdrMsg ->
                    case hdrMsg of
                        HomeClicked        -> updatePage PageHome
                        ProgrammingClicked -> updatePage PageProgramming
                        NewsClicked        -> updatePage PageNews
                        ContactClicked     -> updatePage PageContact

                NewsArrived (Ok nl)    -> { model | newsList = nl }
                NewsArrived (Err e)    -> Debug.log ("ERROR: " ++ toString e) model

                EmailConfirmationArrived (Ok s)  -> 
                    { model | emailConfirmation = Just s }
                EmailConfirmationArrived (Err e) -> 
                    Debug.log ("ERROR: " ++ toString e) model

                EmailSubjectChanged s -> 
                    if Model.checkEmailAddressForValidity s then
                        { model | emailAddressTo = s, isAddrValid = True }
                    else
                        { model | isAddrValid = False }

                EmailBodyChanged s -> { model | emailContent = s }

                SendEmail -> model
        newCmd = 
            case msg of 
                SendEmail -> Api.sendEmailCmd model
                _         -> Cmd.none 
   in 
       (newModel, newCmd)

-- SUBS     

subscriptions : Model -> Sub Msg
subscriptions model = 
    Window.resizes WindowResized

view : Model -> Html.Html Msg
view model =
    Flex.container Flex.Column model.isMobile [Flex.h100] [
        -- HEADER
        div [Flex.w100] [
            Header.createHeaderNavbar model
        ],

        -- MAIN CONTENT
        div [Flex.w100, Flex.flexGrow 1, pageStyle] [
            createPage model
        ]
    ]

main : Program Flags Model Msg
main = Html.programWithFlags {
        init = initialization,
        update = update,
        subscriptions = subscriptions,
        view = view
    }