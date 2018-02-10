import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class, href, type_, rel)

import Window

import Task

import Bootstrap.CDN as BootstrapCDN

import Flex
import Styles exposing (..)
import Model exposing (..)
import Pages exposing (..)
import Header

initialSizeCmd : Cmd Msg
initialSizeCmd = Task.perform WindowResized Window.size

type alias Flags = {
    userAgent: String
}

initialization : Flags -> (Model, Cmd Msg)
initialization flags = (
        {initialModel | userAgent = flags.userAgent, 
                        isMobile  = userAgentCheckMobile flags.userAgent},
        initialSizeCmd
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
   in 
       (newModel, Cmd.none)

-- SUBS     

subscriptions : Model -> Sub Msg
subscriptions model = Window.resizes WindowResized

view : Model -> Html.Html Msg
view model =
    Flex.container Flex.Column model.isMobile [Flex.h100] [
        -- STYLESHEETS
        BootstrapCDN.stylesheet,
        node "link" [href "styles.css", type_ "text/css", rel "stylesheet"] [],

        -- HEADER
        div [Flex.w100] [Header.createHeader model],

        -- MAIN CONTENT
        let (page, chosenStyle) = 
            case model.page of
                PageProgramming -> (pageProgramming model, progPageStyle)
                PageAlgorithms  -> (pageAlgorithms  model, algoPageStyle)
                PageMathematics -> (pageMathematics model, mathPageStyle)

                _               -> (div [] [], class "")
        in
            div [Flex.w100, pageStyle, chosenStyle, Flex.flexGrow 1] [page]
    ]

main : Program Flags Model Msg
main = Html.programWithFlags {
        init = initialization,
        update = update,
        subscriptions = subscriptions,
        view = view
    }