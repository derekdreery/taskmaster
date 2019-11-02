module Style exposing (..)

import Element exposing (Attribute, Element, Color)
import Element.Font as Font
import Element.Input as Input
import Element.Background as Background
import Icon exposing (Icon)


{- On both primary and secondary, text color is white for medium and dark, black for light -}


white : Color
white =
    Element.rgb255 255 255 255


{-| #546e7a Blue Grey 600
-}
primary : Color
primary =
    Element.rgb255 84 110 122


{-| #29434e
-}
primaryDark : Color
primaryDark =
    Element.rgb255 41 67 78


{-| #819ca9
-}
primaryLight : Color
primaryLight =
    Element.rgb255 129 156 169


{-| #9c27b0 Purple 500
-}
secondary : Color
secondary =
    Element.rgb255 156 39 176


{-| #6a0080
-}
secondaryDark : Color
secondaryDark =
    Element.rgb255 106 0 128


{-| #d05ce3
-}
secondaryLight : Color
secondaryLight =
    Element.rgb255 208 92 227


h1 : List (Attribute msg) -> String -> Element msg
h1 list text =
    Element.el
        (Font.size 96 :: Font.light :: list)
        (Element.text text)


type ButtonDecoration
    = FillButton
    | OutlineButton
    | TextButton


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : String
        , icon : Maybe Icon
        , decoration : ButtonDecoration
        }
    -> Element msg
button attrs params =
    let
        paddingLeft =
            case params.icon of
                Just icon ->
                    12

                Nothing ->
                    16

        padding =
            Element.paddingEach
                { top = 0, right = 16, bottom = 0, left = paddingLeft }

        text =
            Element.text <| String.toUpper params.label

        styles =
            case params.decoration of
                FillButton ->
                    [ Background.color secondary
                    , Font.color white
                    ]

                OutlineButton ->
                    []

                TextButton ->
                    [ Font.color secondary
                    ]

        inner =
            case params.icon of
                Just icon ->
                    Element.row [ Element.width Element.fill, Element.spacing 8 ]
                        [ Icon.el2 [] icon, text ]

                Nothing ->
                    text
    in
        Input.button
            (Font.medium
                :: Font.letterSpacing 1.25
                :: Font.size 14
                :: Element.width (Element.shrink |> Element.minimum 64)
                :: Element.height (Element.px 36)
                :: Element.spacing 8
                :: Element.centerY
                :: padding
                :: styles
                ++ attrs
            )
            { onPress = params.onPress, label = inner }
