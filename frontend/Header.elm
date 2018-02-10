module Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Flex

import Model exposing (..)
import Styles exposing (..)

createHeader : Model -> Html Msg
createHeader model =
    if model.isMobile 
    then createHeaderMobile model
    else createHeaderDesktop model

createHeaderDesktop : Model -> Html Msg
createHeaderDesktop model =
    let
        mkItem content page msg =
            let
                selected = model.page == page
                styleSelected = class "header-item-selected"
                styleRegular  = class "header-item-regular"
            in
                Flex.item [headerItemStyle, 
                           if selected then styleSelected else styleRegular,
                           onClick (Header msg)] [text content]
    in
        Flex.container Flex.Row False [headerStyle] 
        -- CHILDREN
        [
        Flex.item [headerItemStyle, brandStyle] [text "Часови по "],
        
        mkItem "Програмирање" PageProgramming ProgrammingClicked,
        mkItem "Алгоритми" PageAlgorithms AlgorithmsClicked,
        mkItem "Математика" PageMathematics MathematicsClicked,

        Flex.item [Flex.flexGrow 1] [],

        mkItem "Дома" PageHome HomeClicked,
        mkItem "Вести" PageNews NewsClicked,
        mkItem "За нас" PageAboutUs AboutUsClicked,
        mkItem "Контакт" PageContact ContactClicked
        ]

createHeaderMobile : Model -> Html Msg
createHeaderMobile model = createHeaderDesktop model