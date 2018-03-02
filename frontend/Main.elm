import Html exposing (..)

import Window

import Flex
import Styles exposing (..)
import Model exposing (..)
import Pages exposing (..)
import Header

-- INIT

type alias Flags = {
    userAgent: String
}

initialization : Flags -> (Model, Cmd Msg)
initialization flags = 
    ({initialModel |
        userAgent = flags.userAgent, 
        isMobile  = userAgentCheckMobile flags.userAgent
      }, Cmd.none)

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
        newCmd = Cmd.none
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
        div [Flex.w100, Flex.flexGrow 1, pageStyle] 
            [createPage model]
    ]

main : Program Flags Model Msg
main = Html.programWithFlags {
        init = initialization,
        update = update,
        subscriptions = subscriptions,
        view = view
    }