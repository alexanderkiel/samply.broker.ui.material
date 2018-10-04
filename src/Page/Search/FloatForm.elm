module Page.Search.FloatForm exposing
    ( Model
    , Msg
    , init
    , toMetricQuery
    , update
    , view
    )

import Data.Interval as Interval exposing (Interval)
import Data.Mdr.DataElement as DataElement exposing (DataElementDetail)
import Data.Search.Criterion as Criterion
import Data.Urn exposing (Urn)
import Html exposing (Html)
import Html.Attributes as Attr
import Material.Button as Button
import Material.Dialog as Dialog
import Material.LayoutGrid as LayoutGrid
import Material.Options as Options
import Material.Select as Select
import Material.TextField as TextField
import Material.TextField.HelperText as HelperText
import Parser.Decimal as Parser exposing (parser)



---- MODEL --------------------------------------------------------------------


type alias Model =
    { elementDetail : DataElementDetail
    , queryTypeSelect : Select.Model
    , lowerBound : Bound
    , upperBound : Bound
    }


type alias Bound =
    { textField : TextField.Model Parser.Context Parser.Problem Float }


init : DataElementDetail -> Maybe (Criterion.MetricQuery Float) -> Model
init elementDetail maybeQuery =
    let
        ( queryType, lowerBound, upperBound ) =
            case maybeQuery of
                Just query ->
                    case query of
                        Criterion.LessThan value ->
                            ( Just "lessThan"
                            , initBound Nothing
                            , initBound <| Just value
                            )

                        Criterion.GreaterThan value ->
                            ( Just "greaterThan"
                            , initBound <| Just value
                            , initBound Nothing
                            )

                        Criterion.Between lowerValue upperValue ->
                            ( Just "between"
                            , initBound <| Just lowerValue
                            , initBound <| Just upperValue
                            )

                        Criterion.Outside lowerValue upperValue ->
                            ( Just "outside"
                            , initBound <| Just lowerValue
                            , initBound <| Just upperValue
                            )

                Nothing ->
                    ( Nothing
                    , initBound Nothing
                    , initBound Nothing
                    )
    in
    { elementDetail = elementDetail
    , queryTypeSelect = Select.init queryType
    , lowerBound = lowerBound
    , upperBound = upperBound
    }


initBound : Maybe Float -> Bound
initBound value =
    { textField = TextField.init parser String.fromFloat value }


toMetricQuery : Model -> Maybe (Criterion.MetricQuery Float)
toMetricQuery { queryTypeSelect, lowerBound, upperBound } =
    queryTypeSelect.value
        |> Maybe.andThen
            (\queryType ->
                case queryType of
                    "lessThan" ->
                        Maybe.map Criterion.LessThan upperBound.textField.value

                    "greaterThan" ->
                        Maybe.map Criterion.GreaterThan lowerBound.textField.value

                    "between" ->
                        Maybe.map2 Criterion.Between
                            lowerBound.textField.value
                            upperBound.textField.value

                    "outside" ->
                        Maybe.map2 Criterion.Outside
                            lowerBound.textField.value
                            upperBound.textField.value

                    _ ->
                        Nothing
            )



---- UPDATE -------------------------------------------------------------------


type Msg
    = QueryTypeMsg Select.Msg
    | LowerBoundMsg BoundMsg
    | UpperBoundMsg BoundMsg


type BoundMsg
    = TextFieldMsg TextField.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        QueryTypeMsg queryTypeMsg ->
            { model | queryTypeSelect = Select.update queryTypeMsg model.queryTypeSelect }

        LowerBoundMsg lowerBoundMsg ->
            { model | lowerBound = updateBound lowerBoundMsg model.lowerBound }

        UpperBoundMsg upperBoundMsg ->
            { model | upperBound = updateBound upperBoundMsg model.upperBound }


updateBound : BoundMsg -> Bound -> Bound
updateBound msg bound =
    case msg of
        TextFieldMsg textFieldMsg ->
            { bound | textField = TextField.update textFieldMsg bound.textField }



---- VIEW ---------------------------------------------------------------------


type BoundModifier
    = Normal
    | Outside


view : Model -> Html Msg
view { elementDetail, queryTypeSelect, lowerBound, upperBound } =
    let
        lowerBoundInput modifier =
            boundInput elementDetail
                (elementDetail.designation
                    ++ (case modifier of
                            Normal ->
                                " should be bigger than this"

                            Outside ->
                                " should be smaller than this"
                       )
                )
                "criterion-float-lower-bound"
                "Lower Bound"
                lowerBound
                |> Html.map LowerBoundMsg

        upperBoundInput modifier =
            boundInput elementDetail
                (elementDetail.designation
                    ++ (case modifier of
                            Normal ->
                                " should be smaller than this"

                            Outside ->
                                " should be bigger than this"
                       )
                )
                "criterion-float-upper-bound"
                "Upper Bound"
                upperBound
                |> Html.map UpperBoundMsg

        placeholder =
            Html.div [ Attr.class "bound-placeholder" ] []

        inputs =
            case queryTypeSelect.value of
                Just "lessThan" ->
                    [ upperBoundInput Normal, placeholder ]

                Just "greaterThan" ->
                    [ lowerBoundInput Normal, placeholder ]

                Just "between" ->
                    [ lowerBoundInput Normal, upperBoundInput Normal ]

                Just "outside" ->
                    [ lowerBoundInput Outside, upperBoundInput Outside ]

                _ ->
                    [ placeholder, placeholder ]
    in
    Dialog.content []
        (description elementDetail :: viewQueryTypeSelect queryTypeSelect :: inputs)


description elementDetail =
    let
        appendUnit str =
            Maybe.map ((++) (str ++ " in ")) (DataElement.getUnit elementDetail)
                |> Maybe.withDefault str
    in
    Html.p []
        [ Html.text <| appendUnit "Metric item" ++ "." ]


viewQueryTypeSelect model =
    let
        option value label =
            Html.option
                [ Attr.value value
                , Attr.selected (model.value == Just value)
                ]
                [ Html.text label ]
    in
    Select.view QueryTypeMsg
        model
        [ Options.class "query-type-select"
        , Options.id "criterion-float-query-type-select"
        , Select.label "Query Type"
        ]
        [ option "lessThan" "Smaller"
        , option "greaterThan" "Bigger"
        , option "between" "Between"
        , option "outside" "Outside"
        ]


boundInput elementDetail helperText id label { textField } =
    Html.div [ Attr.class "bound-input" ]
        [ TextField.view { printer = boundPrinter elementDetail, lift = TextFieldMsg }
            textField
            [ Options.id id
            , TextField.label label
            , Options.style "display" "block"
            ]
            []
        , HelperText.view
            [ HelperText.persistent
            , HelperText.validationMsg
                |> Options.when (not (List.isEmpty textField.parseError))
            ]
            [ case textField.parseError of
                [] ->
                    Html.text helperText

                _ ->
                    Html.text <| "Invalid " ++ elementDetail.designation ++ ". Expecting decimal number."
            ]
        ]


boundPrinter elementDetail =
    case DataElement.getUnit elementDetail of
        Just unit ->
            \val -> String.fromFloat val ++ " " ++ unit

        Nothing ->
            String.fromFloat
