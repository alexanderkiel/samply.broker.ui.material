module Data.Interval exposing
    ( Interval
    , singleton
    , Endpoint(..), closed, decode, encode, endpointValues, endpoints, fromEndpoints, left, right, value
    )

{-|

@docs Interval


# Constructors

@docs singleton

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, resolve)
import Json.Encode as Encode


type Interval value
    = Interval (Endpoint value) (Endpoint value)


type Endpoint value
    = Open value
    | Closed value


singleton : value -> Interval value
singleton val =
    Interval (Closed val) (Closed val)


closed : value -> value -> Interval value
closed leftValue rightValue =
    Interval (Closed leftValue) (Closed rightValue)


fromEndpoints leftEndpoint rightEndpoint =
    Interval leftEndpoint rightEndpoint


endpoints : Interval value -> ( Endpoint value, Endpoint value )
endpoints (Interval l r) =
    ( l, r )


endpointValues : Interval value -> ( value, value )
endpointValues =
    endpoints >> Tuple.mapBoth value value


left : Interval value -> Endpoint value
left (Interval l _) =
    l


right : Interval value -> Endpoint value
right (Interval _ r) =
    r


value : Endpoint value -> value
value endpoint =
    case endpoint of
        Open val ->
            val

        Closed val ->
            val


decode : Decoder value -> Decoder (Interval value)
decode valueDecoder =
    Decode.succeed Interval
        |> required "left" (endpointDecoder valueDecoder)
        |> required "right" (endpointDecoder valueDecoder)


endpointDecoder : Decoder value -> Decoder (Endpoint value)
endpointDecoder valueDecoder =
    let
        toEndpoint bound val =
            case bound of
                "open" ->
                    Decode.succeed <| Open val

                "closed" ->
                    Decode.succeed <| Closed val

                _ ->
                    Decode.fail <| "Unknown bound `" ++ bound ++ "`."
    in
    Decode.succeed toEndpoint
        |> required "bound" Decode.string
        |> required "value" valueDecoder
        |> resolve


encode : (value -> Encode.Value) -> Interval value -> Encode.Value
encode valueEncoder (Interval l r) =
    Encode.object
        [ ( "left", encodeEndpoint valueEncoder l )
        , ( "right", encodeEndpoint valueEncoder r )
        ]


encodeEndpoint : (value -> Encode.Value) -> Endpoint value -> Encode.Value
encodeEndpoint valueEncoder endpoint =
    let
        helper bound val =
            Encode.object
                [ ( "bound", Encode.string bound )
                , ( "value", valueEncoder val )
                ]
    in
    case endpoint of
        Open val ->
            helper "open" val

        Closed val ->
            helper "closed" val
