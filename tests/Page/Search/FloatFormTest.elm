module Page.Search.FloatFormTest exposing (initTest)

import Data.Urn exposing (Urn)
import Data.Mdr.DataElement as DataElement exposing (DataElementDetail)
import Data.Search.Criterion as Criterion exposing (MetricQuery)
import Expect
import Fuzz exposing (Fuzzer)
import Page.Search.FloatForm exposing (..)
import Test exposing (..)
import Browser.Dom as Dom
import Task

initTest : Test
initTest =
    describe "Initializing the form"
        [ describe "with no query"
            [ fuzz dataElementDetailFuzzer "starts with the `LessThan` query type" <|
                \dataElementDetail ->
                    let
                        model =
                            init dataElementDetail Nothing
                    in
                    Expect.equal LessThan model.queryType
            , fuzz dataElementDetailFuzzer "starts with no lower bound value" <|
                \dataElementDetail ->
                    let
                        model =
                            init dataElementDetail Nothing
                    in
                    Expect.equal Nothing model.lowerBound.textField.value
            , fuzz dataElementDetailFuzzer "starts with no upper bound value" <|
                \dataElementDetail ->
                    let
                        model =
                            init dataElementDetail Nothing
                    in
                    Expect.equal Nothing model.upperBound.textField.value
            ]
        , describe "with a query"
            [ fuzz2 dataElementDetailFuzzer
                queryFuzzer
                "uses the query type from the query"
              <|
                \dataElementDetail query ->
                    let
                        model =
                            init dataElementDetail (Just query)
                    in
                    Expect.equal (getQueryType query) model.queryType
            , fuzz2 dataElementDetailFuzzer
                queryFuzzer
                "initializes the lower bound text field with the value from the query"
              <|
                \dataElementDetail query ->
                    let
                        model =
                            init dataElementDetail (Just query)
                    in
                    Expect.equal (getLowerBoundValue query) model.lowerBound.textField.value
            , fuzz2 dataElementDetailFuzzer
                queryFuzzer
                "initializes the upper bound text field with the value from the query"
              <|
                \dataElementDetail query ->
                    let
                        model =
                            init dataElementDetail (Just query)
                    in
                    Expect.equal (getUpperBoundValue query) model.upperBound.textField.value
            ]
        , fuzz2 dataElementDetailFuzzer (Fuzz.maybe queryFuzzer) "init <-> toMetricQuery" <|
            \dataElementDetail maybeQuery ->
                init dataElementDetail maybeQuery
                    |> toMetricQuery
                    |> Expect.equal maybeQuery
        ]


dataElementDetailFuzzer : Fuzzer DataElementDetail
dataElementDetailFuzzer =
    Fuzz.constant
        { id = "foo"
        , designation = "bar"
        , validation = DataElement.Date
        }


queryFuzzer : Fuzzer (MetricQuery Float)
queryFuzzer =
    Fuzz.oneOf
        [ Fuzz.map Criterion.LessThan Fuzz.float
        , Fuzz.map Criterion.GreaterThan Fuzz.float
        , Fuzz.map2 Criterion.Between Fuzz.float Fuzz.float
        ]


getQueryType : MetricQuery a -> QueryType
getQueryType query =
    case query of
        Criterion.LessThan _ ->
            LessThan

        Criterion.GreaterThan _ ->
            GreaterThan

        Criterion.Between _ _ ->
            Between

        Criterion.Outside _ _ ->
            Outside


getLowerBoundValue : MetricQuery a -> Maybe a
getLowerBoundValue query =
    case query of
        Criterion.LessThan _ ->
            Nothing

        Criterion.GreaterThan lb ->
            Just lb

        Criterion.Between lb _ ->
            Just lb

        Criterion.Outside lb _ ->
            Just lb


getUpperBoundValue : MetricQuery a -> Maybe a
getUpperBoundValue query =
    case query of
        Criterion.LessThan ub ->
            Just ub

        Criterion.GreaterThan _ ->
            Nothing

        Criterion.Between _ ub ->
            Just ub

        Criterion.Outside _ ub ->
            Just ub

focusUpperBoundInput : Urn -> Cmd Msg
focusUpperBoundInput id =
    Dom.focus ("criterion-float-upper-bound-" ++ id)
        |> Task.attempt (\_ -> NoOp)
