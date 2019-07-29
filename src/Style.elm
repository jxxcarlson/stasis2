module Style exposing
    ( black
    , blue
    , darkRed
    , gray
    , green
    , lightGreen
    , paletteItem
    , red
    , yellow
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes


paletteItem =
    [ spacing 8, Font.size 48 ]



--
-- COLOR
--


black =
    Element.rgb 0 0 0


blue =
    Element.rgb 0 0 1.0


gray =
    Element.rgba 0.5 0.5 0.5 1.0


green =
    Element.rgb 0 1.0 0


lightGreen =
    Element.rgb 0.9 1.0 0.9


yellow =
    Element.rgb 1.0 1.0 0


red =
    Element.rgb 1.0 0.0 0.0


darkRed =
    Element.rgb 0.5 0.0 0.0
