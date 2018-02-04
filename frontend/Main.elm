import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class, href, type_, rel)

import Window

import Bootstrap.CDN as BootstrapCDN

import Flex
import Styles exposing (..)
import Model exposing (..)
import Pages exposing (..)

initialState : (Model, Cmd msg)
initialState = (initialModel, Cmd.none)

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        WindowResized newSize -> 
            { model | 
              windowSize = newSize,
              screenIsSmartphone = newSize.width <= Flex.phoneWidthThreshold,
              screenIsPortrait = newSize.height >= newSize.width}
        _ ->
            let newPage = case msg of 
                ProgrammingClicked -> PageProgramming
                AlgorithmsClicked  -> PageAlgorithms
                MathematicsClicked -> PageMathematics
                _                  -> model.page
            in
                { model | page = newPage}
    , Cmd.none)

-- SUBS     

subscriptions : Model -> Sub Msg
subscriptions model = Window.resizes WindowResized

-- Change header button background to blue if it is active
bgActive : Model -> Page -> Attribute Msg
bgActive model page = 
    if model.page == page 
    then style [("background-color", "#1351d8")]
    else style [("background-color", "#2f4f4f")] 

-- VIEW
createHeader : Model -> Html.Html Msg
createHeader model =
    Flex.container Flex.Auto model.windowSize [Flex.w100, headerStyle] [
        Flex.item [Flex.flexBasis 20, brandStyle] [text "Часови"],
        Flex.item [Flex.flexBasis 5] [], -- separator
        Flex.item [Flex.flexBasis 25, class "text-center", 
             bgActive model PageProgramming,
             programmingStyle, onClick ProgrammingClicked] 
            [text "Програмирање"],
        Flex.item [Flex.flexBasis 25, class "text-center",
             bgActive model PageAlgorithms,
             algorithmsStyle, onClick AlgorithmsClicked] 
            [text "Алгоритми"],
        Flex.item [Flex.flexBasis 25, class "text-center",
            bgActive model PageMathematics,
             mathStyle, onClick MathematicsClicked] 
            [text "Математика"]
    ]

createContact : Model -> Html.Html Msg
createContact model = 
    let
        class1 = if model.screenIsSmartphone then "text-center" else "text-left"
        class2 = if model.screenIsSmartphone then "text-center" else "text-right"
        colwhite = style [("color", "white")]
    in
        Flex.container Flex.Auto model.windowSize [contactStyle, Flex.w100] [
            Flex.item [class class1, Flex.flexBasis 50] 
                [a [href "tel:076648258", colwhite] [text "076 648 258"]],
            Flex.item [class class2, Flex.flexBasis 50] 
                [a [href "mailto:aleksandar.jovanov.1995@gmail.com", colwhite] 
                    [text "aleksandar.jovanov.1995@gmail.com"]]
        ]

view : Model -> Html.Html Msg
view model =
    Flex.container Flex.Column model.windowSize [Flex.h100] [
        -- STYLESHEETS
        BootstrapCDN.stylesheet,
        node "link" [href "styles.css", type_ "text/css", rel "stylesheet"] [],

        -- HEADER
        div [Flex.w100] [createHeader model],

        -- MAIN CONTENT
        let (page, chosenStyle) = 
            case model.page of
                PageProgramming -> (pageProgramming model, progPageStyle)
                PageAlgorithms  -> (pageAlgorithms  model, algoPageStyle)
                PageMathematics -> (pageMathematics model, mathPageStyle)
        in
            div [Flex.w100, pageStyle, chosenStyle, Flex.flexGrow 1] [page],

        -- CONTACT
        div [Flex.w100] [createContact model]
    ]

main : Program Never Model Msg
main = Html.program {
        init = initialState,
        update = update,
        subscriptions = subscriptions,
        view = view
    }