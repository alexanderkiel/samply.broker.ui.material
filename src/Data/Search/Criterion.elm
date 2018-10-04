module Data.Search.Criterion exposing
    ( Criterion
    , MetricQuery(..)
    , Query(..)
    , decoder
    , encodeWithSearchId
    )

import Data.Interval as Interval exposing (Interval)
import Data.Urn as Urn exposing (Urn)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, resolve)
import Json.Encode as Encode
import Time exposing (Posix)


type alias Criterion =
    { mdrKey : Urn
    , query : Query
    }


type Query
    = Enumerated (List String)
    | Date (Maybe Posix) (Maybe Posix)
    | Float (MetricQuery Float)


type MetricQuery a
    = LessThan a
    | GreaterThan a
    | Between a a
    | Outside a a


decoder : Decoder Criterion
decoder =
    let
        toCriterion mdrKey type_ =
            queryDecoder type_
                |> Decode.map
                    (\query ->
                        { mdrKey = mdrKey
                        , query = query
                        }
                    )
    in
    Decode.succeed toCriterion
        |> required "mdr-key" Urn.decoder
        |> required "type" Decode.string
        |> resolve


queryDecoder : String -> Decoder Query
queryDecoder type_ =
    case type_ of
        "enumerated" ->
            Decode.succeed Enumerated
                |> required "selected-values" (Decode.list Decode.string)

        "float" ->
            Decode.succeed Float
                |> required "metric-query" metricQueryDecoder

        _ ->
            Decode.fail <| "Unknown criterion type `" ++ type_ ++ "`."


metricQueryDecoder : Decoder (MetricQuery Float)
metricQueryDecoder =
    let
        toMetricQuery type_ =
            case type_ of
                "less-than" ->
                    Decode.succeed LessThan
                        |> required "upper-bound" Decode.float

                "greater-than" ->
                    Decode.succeed GreaterThan
                        |> required "lower-bound" Decode.float

                "between" ->
                    Decode.succeed Between
                        |> required "lower-bound" Decode.float
                        |> required "upper-bound" Decode.float

                "outside" ->
                    Decode.succeed Outside
                        |> required "lower-bound" Decode.float
                        |> required "upper-bound" Decode.float

                _ ->
                    Decode.fail <| "Unknown metric query type `" ++ type_ ++ "`."
    in
    Decode.succeed toMetricQuery
        |> required "type" Decode.string
        |> resolve


encodeWithSearchId : String -> Criterion -> Encode.Value
encodeWithSearchId searchId { mdrKey, query } =
    let
        commonEntries type_ =
            [ ( "search-id", Encode.string searchId )
            , ( "mdr-key", Encode.string mdrKey )
            , ( "type", Encode.string type_ )
            ]
    in
    case query of
        Enumerated selectedValues ->
            Encode.object <|
                commonEntries "enumerated"
                    ++ [ ( "selected-values"
                         , Encode.list Encode.string selectedValues
                         )
                       ]

        Date maybeStart maybeEnd ->
            let
                timeEntry key val =
                    [ ( key, Time.posixToMillis val |> Encode.int ) ]

                maybeTimeEntry key val =
                    Maybe.map (timeEntry key) val
                        |> Maybe.withDefault []
            in
            Encode.object <|
                commonEntries "date"
                    ++ maybeTimeEntry "start" maybeStart
                    ++ maybeTimeEntry "end" maybeEnd

        Float metricQuery ->
            Encode.object <|
                commonEntries "float"
                    ++ [ ( "metric-query", encodeMetricQuery metricQuery ) ]


encodeMetricQuery query =
    Encode.object <|
        case query of
            LessThan x ->
                [ ( "type", Encode.string "less-than" )
                , ( "upper-bound", Encode.float x )
                ]

            GreaterThan x ->
                [ ( "type", Encode.string "greater-than" )
                , ( "lower-bound", Encode.float x )
                ]

            Between l u ->
                [ ( "type", Encode.string "between" )
                , ( "lower-bound", Encode.float l )
                , ( "upper-bound", Encode.float u )
                ]

            Outside l u ->
                [ ( "type", Encode.string "outside" )
                , ( "lower-bound", Encode.float l )
                , ( "upper-bound", Encode.float u )
                ]
