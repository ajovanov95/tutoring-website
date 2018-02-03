import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class, href, type_, rel)

import Bootstrap.CDN as CDN

import Flex exposing (..)
import Styles exposing (..)
import Model exposing (..)
import Pages exposing (..)

initialState : (Model, Cmd msg)
initialState = ({ page = PageProgramming }, Cmd.none)

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newPage = case msg of
            ProgrammingClicked -> PageProgramming
            AlgorithmsClicked  -> PageAlgorithms
            MathematicsClicked -> PageMathematics
    in
        ({ model | page = newPage }, Cmd.none)

-- SUBS     

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- Change header button background to blue if it is active
bgActive : Model -> Page -> Attribute Msg
bgActive model page = 
    if model.page == page 
    then style [("background-color", "#1351d8")]
    else style [("background-color", "#2f4f4f")] 

-- VIEW
createHeader : Model -> Html.Html Msg
createHeader model =
    horizontalContainer [w100, headerStyle, flexNoWrap] [
        div [flexBasis "20%", nonSelectable, brandStyle] [text "Учиме и положуваме"],
        div [flexBasis "5%"] [], -- separator
        div [flexBasis "25%", nonSelectable, class "text-center", 
             bgActive model PageProgramming,
             programmingStyle, onClick ProgrammingClicked] 
            [text "Програмирање"],
        div [flexBasis "25%", nonSelectable, class "text-center",
             bgActive model PageAlgorithms,
             algorithmsStyle, onClick AlgorithmsClicked] 
            [text "Алгоритми"],
        div [flexBasis "25%", nonSelectable, class "text-center",
            bgActive model PageMathematics,
             mathStyle, onClick MathematicsClicked] 
            [text "Математика"]
    ]

createContact : Model -> Html.Html Msg
createContact _ = 
    horizontalContainer [contactStyle, flexWrap, w100] [
        div [class "text-left", flexBasis "50%"] 
            [text "076 648 258"],
        div [class "text-right", flexBasis "50%"] 
            [text "aleksandar.jovanov.1995@gmail.com"]
    ]

view : Model -> Html.Html Msg
view model =
    verticalContainer [h100] [
        -- STYLESHEETS
        CDN.stylesheet,
        node "link" [href "styles.css", type_ "text/css", rel "stylesheet"] [],

        -- HEADER
        div [w100] [createHeader model],

        -- MAIN CONTENT
        let (page, chosenStyle) = 
            case model.page of
                PageProgramming -> (pageProgramming model, progPageStyle)
                PageAlgorithms  -> (pageAlgorithms  model, algoPageStyle)
                PageMathematics -> (pageMathematics model, mathPageStyle)
        in
            div [w100, pageStyle, chosenStyle, flexGrow "1"] [page],

        -- CONTACT
        div [w100] [createContact model]
    ]

main : Program Never Model Msg
main = Html.program {
        init = initialState,
        update = update,
        subscriptions = subscriptions,
        view = view
    }