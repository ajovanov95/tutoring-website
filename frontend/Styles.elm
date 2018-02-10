module Styles exposing (..)

import Html
import Html.Attributes exposing (style, class, id)

type alias Style msg = Html.Attribute msg

headerStyle : Style msg
headerStyle = id "header"

headerItemStyle : Style msg
headerItemStyle = class "header-item"

headerItemSelectedStyle : Style msg
headerItemSelectedStyle = class "header-item-selected"

headerSepStyle : Style msg
headerSepStyle = class "header-separator"

brandStyle : Style msg
brandStyle = id "brand"

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

cardStyle : Style msg
cardStyle = class "item-card"

cardLinkStyle : Style msg
cardLinkStyle = class "item-card-link"

progPageStyle : Style msg
progPageStyle = id "programming-page-style"

algoPageStyle : Style msg 
algoPageStyle = id "algorithms-page-style"

mathPageStyle : Style msg
mathPageStyle = id "mathematics-page-style"
