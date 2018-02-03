module Flex exposing (..)

import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (style)

(:>) : String -> String -> (String, String)
(:>) a b = (a, b)

horizontalContainer : List (Attribute msg) -> List (Html msg) -> Html msg
horizontalContainer attrs children = 
    let
        s = style ["display" :> "flex", 
                   "flex-direction" :> "row"]
    in
        div ([s] ++ attrs) children

verticalContainer : List (Attribute msg) -> List (Html msg) -> Html msg
verticalContainer attrs children = 
    let
        s = style ["display" :> "flex", 
                   "flex-direction" :> "column"]
    in
        div ([s] ++ attrs) children

flexBasis : String -> Html.Attribute msg
flexBasis x = style ["flex-basis" :> x]

flexGrow : String -> Html.Attribute msg
flexGrow x = style ["flex-grow" :> x]

flexWrap : Attribute msg
flexWrap   = style ["flex-wrap" :> "wrap"]

flexNoWrap : Attribute msg
flexNoWrap = style ["flex-wrap" :> "no-wrap"]

w100 : Attribute msg
w100 = style ["width" :> "100%"]

h100 : Attribute msg
h100 = style ["height" :> "100%"]