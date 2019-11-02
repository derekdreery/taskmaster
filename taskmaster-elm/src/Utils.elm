module Utils exposing (..)

import Element exposing (Element, Attribute, Color)
import Debug
import Array exposing (Array)
import Parser exposing (Parser, Step(..), (|.), (|=))


ex : Attribute msg
ex =
    Element.explain Debug.todo


unreachable : a -> a
unreachable =
    Debug.log "error - this code path should be unreachable"


arraySingleton : a -> Array a
arraySingleton val =
    Array.push val Array.empty


todo : a
todo =
    Debug.todo ""


maybeEl : Maybe (Element msg) -> Element msg
maybeEl maybeEl_ =
    case maybeEl_ of
        Just el ->
            el

        Nothing ->
            Element.none


removeFromArray : Int -> Array a -> Array a
removeFromArray i a =
    let
        a1 =
            Array.slice 0 i a

        a2 =
            Array.slice (i + 1) (Array.length a) a
    in
        Array.append a1 a2


mapArrayAt : (a -> a) -> Int -> Array a -> Array a
mapArrayAt mapper idx array =
    let
        length =
            Array.length array

        a1 =
            Array.slice 0 idx array

        aMapped =
            Maybe.map mapper (Array.get idx array)

        a2 =
            Array.slice (idx + 1) length array
    in
        case aMapped of
            Just val ->
                Array.append (Array.push val a1) a2

            Nothing ->
                Array.append a1 a2


{-| Parse a color from its hex repr. Return a horrible red color if couldn't parse.
-}
fromHex : String -> Color
fromHex colorStr =
    Element.rgb255 255 0 0


{-| Parse a color from its hex repr
-}
fromHex_ : Parser Color
fromHex_ =
    Parser.succeed Element.rgb255
        |. optional () (Parser.symbol "#")
        |= Parser.andThen hex (take 2)
        |= Parser.andThen hex (take 2)
        |= Parser.andThen hex (take 2)


{-| Parese a hex number of the form `4a3fde` etc.
-}
hex : String -> Parser Int
hex =



hexLoop : Int -> Parser (Step Int Int)
hexLoop acc =
    Parser.oneOf
        [ Parser.succeed (\val -> Loop (acc + val))
            |= hexChar
        , Parser.succeed () |> Parser.map (\_ -> Done acc)
        ]


{-| Runs the parser over the given number of characters
-}
take : Int -> Parser String
take count =
    if count <= 0 then
        Parser.problem "cannot take <= 0 chars"
    else
        let
            source = Parser.get
        in

takeLoop : Int -> Parser (Step Int ())
takeLoop acc =
    if acc == 0 then
        Parser.succeed (Done ())
    else
        Parser

    Parser.oneOf
        [ Parser.succeed (\val -> Loop (acc + val))
            |= hexChar
        , Parser.succeed () |> Parser.map (\_ -> Done acc)
        ]


{-| Optionally parse an `a`, with a default value if that was not possible
-}
optional : a -> Parser a -> Parser a
optional default parser =
    Parser.oneOf [ parser, Parser.succeed default ]
