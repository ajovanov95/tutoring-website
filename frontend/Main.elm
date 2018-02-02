import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class, href, type_, rel)

import Bootstrap.CDN as CDN

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col

import Styles exposing (..)
import Model exposing (..)
import Pages exposing (..)

initialState : (Model, Cmd msg)
initialState = ({ page = PageMathematics }, Cmd.none)

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

-- VIEW
createHeader : Model -> Html.Html Msg
createHeader _ =
    Grid.row [Row.centerXs, Row.attrs [headerStyle]] [
        Grid.col [Col.xs3, Col.attrs [nonSelectable]]
        [
            text "Учиме и положуваме"
        ],
        Grid.col [Col.xs3, Col.attrs [programmingStyle, nonSelectable,
                                      onClick ProgrammingClicked]
                    ] [text "Програмирање"],
        Grid.col [Col.xs3, Col.attrs [algorithmsStyle, nonSelectable,
                                      onClick AlgorithmsClicked]
                    ] [text "Алгоритми"],
        Grid.col [Col.xs3, Col.attrs [mathStyle, nonSelectable,
                                      onClick MathematicsClicked]
                    ][text "Mатематика"]
    ]

createContact : Model -> Html.Html Msg
createContact _ = 
    Grid.row [Row.attrs [contactStyle]] [
        Grid.col [Col.xs6, Col.attrs [class "text-left"]] [
            text "Telephone : 076 648 258"
        ],
        Grid.col [Col.xs6, Col.attrs [class "text-right"]] [
            text "E-mail : aleksandar.jovanov.1995@gmail.com"
        ]
    ]

createFooter : Model -> Html.Html Msg
createFooter _ =
    Grid.row [Row.attrs [footerStyle]] [
        Grid.col [Col.xs12] [
            text "Made by Aleksandar Jovanov with Elm and Bootstrap"
        ]
    ]

view : Model -> Html.Html Msg
view model =
 Grid.containerFluid [class "d-flex flex-column h-100"] 
 [
     CDN.stylesheet,
     node "link" [href "styles.css", type_ "text/css", rel "stylesheet"] [],
     div [class "d-flex flex-column flex-grow"] [
        createHeader model,
        Grid.row [Row.attrs [class "bg-info h-100 flex-grow"]] 
        [
            Grid.col [Col.xs12] [
                case model.page of 
                    PageProgramming -> pageProgramming model
                    PageAlgorithms  -> pageAlgorithms model
                    PageMathematics -> pageMathematics model
             ]
        ],
        createContact model,
        createFooter model
     ]
     
 ]

main : Program Never Model Msg
main = Html.program {
        init = initialState,
        update = update,
        subscriptions = subscriptions,
        view = view
    }