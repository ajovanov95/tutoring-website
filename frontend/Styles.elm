module Styles exposing (..)

import Html
import Html.Attributes exposing (style, class, id)

type alias Style msg = Html.Attribute msg

pageStyle : Style msg
pageStyle = class "page-style"

cardStyle : Style msg
cardStyle = class "item-card"
