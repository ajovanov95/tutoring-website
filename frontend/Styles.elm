module Styles exposing (..)

import Html
import Html.Attributes exposing (style, class, id)

type alias Style msg = Html.Attribute msg

nonSelectable : Style msg
nonSelectable = class "non-selectable"

headerStyle : Style msg
headerStyle = id "header"

footerStyle : Style msg
footerStyle = id "footer"

contactStyle : Style msg 
contactStyle = id "contact"

programmingStyle : Style msg
programmingStyle = id "programming-button"

algorithmsStyle : Style msg
algorithmsStyle = id "algorithms-button"

mathStyle : Style msg
mathStyle = id "mathematics-button"

pageStyle : Style msg
pageStyle = class "page-style"

mathPageStyle : Style msg
mathPageStyle = id "mathematics-page-style"