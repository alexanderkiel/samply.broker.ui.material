module Parser.Decimal exposing (Context, Parser, Problem(..), parser)

import Parser.Advanced as Parser exposing ((|.), (|=), Token(..), map, oneOf, succeed, symbol)


type alias Parser a =
    Parser.Parser Context Problem a


type Context
    = Context


type Problem
    = ExpectingDigit
    | ExpectingMinusSign
    | ExpectingComma
    | ExpectingPeriod
    | ExpectingEnd


{-| Parser for decimal values with either `.` or `,` as decimal separator.
-}
parser : Parser Float
parser =
    oneOf
        [ Parser.backtrackable decimal
        , integral
        , fractional
        ]


integral : Parser Float
integral =
    succeed toFloat
        |= int
        |. Parser.end ExpectingDigit


decimal : Parser Float
decimal =
    let
        toFloat intPart fractionPart =
            Basics.toFloat intPart
                + sureToFloat fractionPart
                / Basics.toFloat (10 ^ String.length fractionPart)
    in
    succeed toFloat
        |= int
        |. separator
        |= digits
        |. Parser.end ExpectingDigit


separator =
    oneOf
        [ symbol <| Token "," ExpectingComma
        , symbol <| Token "." ExpectingPeriod
        ]


fractional : Parser Float
fractional =
    let
        toFloat fractionPart =
            sureToFloat fractionPart
                / Basics.toFloat (10 ^ String.length fractionPart)
    in
    succeed toFloat
        |. separator
        |= atLeastOneDigit
        |. Parser.end ExpectingEnd


int : Parser Int
int =
    succeed ()
        |. oneOf
            [ symbol <| Token "-" ExpectingMinusSign
            , succeed ()
            ]
        |. Parser.chompIf Char.isDigit ExpectingDigit
        |. Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> map sureToInt


{-| Eats as many digits as possible.
-}
digits : Parser String
digits =
    Parser.getChompedString <| Parser.chompWhile Char.isDigit


atLeastOneDigit : Parser String
atLeastOneDigit =
    succeed ()
        |. Parser.chompIf Char.isDigit ExpectingDigit
        |. Parser.chompWhile Char.isDigit
        |> Parser.getChompedString


sureToInt =
    String.toInt >> Maybe.withDefault 0


sureToFloat =
    String.toFloat >> Maybe.withDefault 0
