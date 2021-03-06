module Layout.Grid exposing (GridSpecification, autoSpecification, container, item)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias GridSpecification = {
    columns : String,
    gap     : String
}

(:>) : String -> String -> (String, String)
(:>) a b = (a, b)

type alias IsMobile = Bool

autoSpecification : IsMobile -> GridSpecification
autoSpecification isMobile =
    if isMobile then
        { columns = "1fr", gap = "1em" }
    else
        { columns = "1fr 1fr", gap = "1em" }

container : GridSpecification -> List (Attribute msg) -> List (Html msg) -> Html msg
container spec attrs children = 
    let
        s = style [
            "display" :> "grid", 
            "grid-template-columns" :> spec.columns,
            "grid-gap" :> spec.gap
        ]
    in
        div (s :: attrs) children

item : List (Attribute msg) -> List (Html msg) -> Html msg
item = div