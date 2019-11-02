module Icon exposing (Icon(..), el, el2, button, blank)

{-| Material icons
-}

import Element exposing (Element, Attribute, html, px)
import Element.Input as Input
import Element.Font as Font
import Html
import Html.Attributes exposing (class)


type Icon
    = Add
    | ExpandLess
    | ExpandMore
    | ArrowForwardIos
    | KeyboardArrowRight
    | Cancel
    | Edit


toString : Icon -> String
toString iconType =
    case iconType of
        Add ->
            "add"

        ExpandLess ->
            "expand_less"

        ExpandMore ->
            "expand_more"

        KeyboardArrowRight ->
            "keyboard_arrow_right"

        ArrowForwardIos ->
            "arrow_forward_ios"

        Cancel ->
            "cancel"

        Edit ->
            "edit"


el : List (Attribute msg) -> Icon -> Element msg
el attrs iconType =
    icon_ iconType


el2 : List (Attribute msg) -> Icon -> Element msg
el2 attrs iconType =
    Element.el
        (Font.size 24
            :: Font.family [ Font.typeface "Material Icons" ]
            :: Font.regular
            :: Font.unitalicized
            :: attrs
        )
        (Element.text <| toString iconType)


button : List (Attribute msg) -> Icon -> Maybe msg -> Element msg
button attrs iconType onPress =
    Input.button attrs
        { onPress = onPress
        , label = icon_ iconType
        }


blank : List (Attribute msg) -> Element msg
blank attrs =
    Element.el attrs <| Element.el [ Element.width <| px 24, Element.height <| px 24 ] Element.none


icon_ : Icon -> Element msg
icon_ iconType =
    html <| Html.div [ class "material-icons" ] [ Html.text <| toString iconType ]
