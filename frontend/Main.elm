import Html exposing (..)
import Html.Attributes exposing (style, class, href, type_, rel)

import Window

import Task

import Bootstrap.CDN as BootstrapCDN
import Bootstrap.Navbar as Navbar

import Flex
import Styles exposing (..)
import Model exposing (..)
import Pages exposing (..)
import Header

-- INIT

type alias Flags = {
    userAgent: String
}

initialSizeCmd : Cmd Msg
initialSizeCmd = Task.perform WindowResized Window.size

initialization : Flags -> (Model, Cmd Msg)
initialization flags = 
    let
        (navbarState, navbarCmd) = Navbar.initialState NavbarMsg
    in (
        {initialModel | userAgent = flags.userAgent, 
                        isMobile  = userAgentCheckMobile flags.userAgent,
                        navbarState = navbarState},
        Cmd.batch [initialSizeCmd, navbarCmd]
    )

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
                        ProgrammingClicked -> updatePage PageProgramming
                        AlgorithmsClicked  -> updatePage PageAlgorithms
                        MathematicsClicked -> updatePage PageMathematics

                        HomeClicked        -> updatePage PageHome
                        NewsClicked        -> updatePage PageNews
                        AboutUsClicked     -> updatePage PageAboutUs
                        ContactClicked     -> updatePage PageContact
                NavbarMsg newState -> { model | navbarState = newState }
        newCmd = Cmd.none
   in 
       (newModel, newCmd)

-- SUBS     

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch 
        [Window.resizes WindowResized,
         Navbar.subscriptions model.navbarState NavbarMsg]

view : Model -> Html.Html Msg
view model =
    Flex.container Flex.Column model.isMobile [Flex.h100] [
        -- STYLESHEETS
        BootstrapCDN.stylesheet,
        -- BootstrapCDN.fontAwesome,
        node "link" [href "styles.css", type_ "text/css", rel "stylesheet"] [],

        -- HEADER
        div [Flex.w100] [
            Header.createHeaderNavbar model
        ],

        -- MAIN CONTENT
        div [Flex.w100, pageStyle, Flex.flexGrow 1] [createPage model]
    ]

main : Program Flags Model Msg
main = Html.programWithFlags {
        init = initialization,
        update = update,
        subscriptions = subscriptions,
        view = view
    }