import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Bootstrap.CDN as CDN

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col

import Styles exposing (..)

-- MODEL

type Page = Programming | Algorithms | Mathematics

type alias Model = Page

type Msg = ProgrammingClicked | AlgorithmsClicked | MathematicsClicked

initialState : (Model, Cmd msg)
initialState = (Programming, Cmd.none)

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProgrammingClicked -> (Programming, Cmd.none)
        AlgorithmsClicked  -> (Algorithms, Cmd.none)
        MathematicsClicked -> (Mathematics, Cmd.none)

-- SUBS     

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

dynamicBgColour : Model -> Html.Attribute Msg
dynamicBgColour model =
    let
        color_ = 
            case model of 
                Programming -> seaGreen
                Algorithms  -> irishGreen
                Mathematics -> jungleGreen
    in
        style [("background-color", color_)]

createHeader : Model -> Html.Html Msg
createHeader model =
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

createContent : Model -> Html.Html Msg
createContent model =
    case model of 
        Programming ->
            text "Programming"
        Algorithms ->
            text "Algorithms"
        Mathematics ->
            text "Mathematics"

view : Model -> Html.Html Msg
view model =
 Grid.containerFluid [dynamicBgColour model] 
 [
     CDN.stylesheet,
     createHeader model,
     Grid.row [Row.middleXs, Row.centerXs] 
     [
         Grid.col [Col.xs12] [
             createContent model
         ]
     ]
 ]

main : Program Never Model Msg
main = Html.program {
        init = initialState,
        update = update,
        subscriptions = subscriptions,
        view = view
    }