module Styles exposing (..)

import Html
import Html.Attributes exposing (style, class, id)

type alias Style msg = Html.Attribute msg

pageStyle : Style msg
pageStyle = class "page-style"

cardStyle : Style msg
cardStyle = class "item-card"

(:>) : String -> String -> (String, String)
(:>) a b = (a, b)

style_ x = [x] |> style

-- Example
-- "width" :> "50px" :: "height" :> "60px" :: []