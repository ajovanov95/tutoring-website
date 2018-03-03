module Layout.Flex exposing (FlexDirection(Row, Column, Auto),
                            container,
                            item,
                            flexBasis,
                            flexGrow,
                            flexWrap,
                            flexNoWrap,
                            w100, h100)

import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (style)

import Styles exposing (..)

-- Auto direction is row when possible, columns otherwise
type FlexDirection = Row | Column | Auto

type alias IsMobile = Bool

container :  FlexDirection -> 
             IsMobile -> 
             List (Attribute msg) -> List (Html msg) -> Html msg
container direction isMobile attrs children = 
    let 
        d = case direction of
                Row -> "row"
                Column -> "column"
                Auto  -> if isMobile then "column" else "row"
        s = style ["display" :> "flex", "flex-direction" :> d]
    in
        div (s :: attrs) children

-- div is just a stupid name
item : List (Attribute msg) -> List (Html msg) -> Html msg
item = div

flexBasis : Int -> Html.Attribute msg
flexBasis x = style ["flex-basis" :> ((toString x) ++ "%")]

flexGrow : Int -> Html.Attribute msg
flexGrow x = style ["flex-grow" :> (toString x)]

flexWrap : Attribute msg
flexWrap = style ["flex-wrap" :> "wrap"]

flexNoWrap : Attribute msg
flexNoWrap = style ["flex-wrap" :> "no-wrap"]

w100 : Attribute msg
w100 = style ["width" :> "100%"]

h100 : Attribute msg
h100 = style ["height" :> "100%"]