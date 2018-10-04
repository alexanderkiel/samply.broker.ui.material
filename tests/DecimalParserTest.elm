module DecimalParserTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, oneOf, constant)
import Parser.Advanced as Parser
import Parser.Decimal exposing (Problem(..), parser)
import Test exposing (..)


suite : Test
suite =
    describe "The decimal parser"
        [ test "fails on empty string" <|
            \_ ->
                case Parser.run parser "" of
                    Ok _ ->
                        Expect.fail "expected parser to fail"

                    Err [] ->
                        Expect.fail "expected at least one dead end"

                    Err ({ problem } :: _) ->
                        Expect.equal ExpectingDigit problem
        , test "fails on invalid start" <|
            \_ ->
                case Parser.run parser "a" of
                    Ok _ ->
                        Expect.fail "expected parser to fail"

                    Err [] ->
                        Expect.fail "expected at least one dead end"

                    Err ({ problem } :: _) ->
                        Expect.equal ExpectingDigit problem
        , fuzz int "fails on invalid end" <|
            \i ->
                case Parser.run parser <| String.fromInt i ++ "a" of
                    Ok _ ->
                        Expect.fail "expected parser to fail"

                    Err [] ->
                        Expect.fail "expected at least one dead end"

                    Err ({ problem } :: _) ->
                        Expect.equal ExpectingDigit problem
        , fuzz decimalSeparator "fails on decimal separator only" <|
            \sep ->
                case Parser.run parser sep of
                    Ok _ ->
                        Expect.fail "expected parser to fail"

                    Err [] ->
                        Expect.fail "expected at least one dead end"

                    Err ({ problem } :: _) ->
                        Expect.equal ExpectingDigit problem
        , fuzz int "succeeds on integer string" <|
            \i ->
                case Parser.run parser <| String.fromInt i of
                    Ok j ->
                        Expect.equal i (round j)

                    Err _ ->
                        Expect.fail "expected parser to succeed"
        , fuzz3 int decimalSeparator (intRange 1 9) "succeeds on decimal string" <|
            \i sep d ->
                case Parser.run parser <| String.fromInt i ++ sep ++ String.fromInt d of
                    Ok k ->
                        Expect.all
                            [ Expect.lessThan (toFloat (i + 1))
                            , Expect.greaterThan (toFloat i)
                            ]
                         k

                    Err _ ->
                        Expect.fail "expected parser to succeed"
        , fuzz2 decimalSeparator (intRange 1 9) "succeeds on decimal string without leading zero" <|
            \sep d ->
                case Parser.run parser <| sep ++ String.fromInt d of
                    Ok k ->
                        Expect.all
                            [ Expect.lessThan 1
                            , Expect.greaterThan 0
                            ]
                         k

                    Err _ ->
                        Expect.fail "expected parser to succeed"
        ]

decimalSeparator : Fuzzer String
decimalSeparator =
    oneOf [ constant ".", constant ","]
