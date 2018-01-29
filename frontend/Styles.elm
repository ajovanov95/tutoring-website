module Styles exposing (headerStyle, 
                        programmingStyle,
                        algorithmsStyle,
                        mathStyle,
                        nonSelectable,
                        -- colors
                        fordBlue,
                        mintGreen,

                        irishGreen,
                        jungleGreen,
                        seaGreen
                        )

import Html
import Html.Attributes exposing (style)

-- small utility to make writing styles easier
(:>) : a -> b -> (a, b)
(:>) x y = (x, y)

fordBlue : String
fordBlue = "#1351d8"

mintGreen : String
mintGreen = "#98FB98"

irishGreen : String
irishGreen = "#009E60"

jungleGreen : String
jungleGreen = "#29AB87"

seaGreen : String
seaGreen = "#2E8B57"

graySlate : String
graySlate = "#2f4f4f"

fontSize20 : (String, String)
fontSize20 = "font-size" :> "20pt"

nonSelectable : Html.Attribute msg
nonSelectable = 
    style [
        "-webkit-touch-callout" :> "none",
        "-webkit-user-select" :> "none",
        "-khtml-user-select" :> "none",
        "-moz-user-select" :> "none",
        "-ms-user-select" :> "none",
        "user-select" :> "none"
    ]

type alias Style msg = Html.Attribute msg

headerStyle : Style msg
headerStyle = 
    style [
        "background-color" :> graySlate,
        "font-size" :> "18pt",
        "color" :> "white",
        "padding" :> "5pt"
    ]

programmingStyle : Style msg
programmingStyle = 
    style [
        fontSize20,
        "background-color" :> seaGreen
    ]

algorithmsStyle : Style msg
algorithmsStyle =
    style [
        fontSize20,
        "background-color" :> irishGreen
    ]

mathStyle : Style msg
mathStyle = 
    style [
        fontSize20,
        "background-color" :> jungleGreen
    ]