module Page.Search.FloatForm exposing
    ( Model
    , QueryType(..)
    , init
    , toMetricQuery
    , Msg(..)
    , update
    , view
    )

{-| This module contains a form were the user can create or edit a
`Criterion.MetricQuery` with float values.


# Model

@docs Model
@docs QueryType
@docs init
@docs toMetricQuery


# Update

@docs Msg
@docs update


# View

@docs view

-}

import Browser.Dom as Dom
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
import Task



---- MODEL --------------------------------------------------------------------


type alias Model =
    { elementDetail : DataElementDetail
    , queryType : QueryType
    , lowerBound : Bound
    , upperBound : Bound
    }


type QueryType
    = LessThan
    | GreaterThan
    | Between
    | Outside


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
                            ( LessThan
                            , initBound Nothing
                            , initBound <| Just value
                            )

                        Criterion.GreaterThan value ->
                            ( GreaterThan
                            , initBound <| Just value
                            , initBound Nothing
                            )

                        Criterion.Between lowerValue upperValue ->
                            ( Between
                            , initBound <| Just lowerValue
                            , initBound <| Just upperValue
                            )

                        Criterion.Outside lowerValue upperValue ->
                            ( Outside
                            , initBound <| Just lowerValue
                            , initBound <| Just upperValue
                            )

                Nothing ->
                    ( LessThan
                    , initBound Nothing
                    , initBound Nothing
                    )
    in
    { elementDetail = elementDetail
    , queryType = queryType
    , lowerBound = lowerBound
    , upperBound = upperBound
    }


initBound : Maybe Float -> Bound
initBound value =
    { textField = TextField.init parser String.fromFloat value }


toMetricQuery : Model -> Maybe (Criterion.MetricQuery Float)
toMetricQuery { queryType, lowerBound, upperBound } =
    case queryType of
        LessThan ->
            Maybe.map Criterion.LessThan upperBound.textField.value

        GreaterThan ->
            Maybe.map Criterion.GreaterThan lowerBound.textField.value

        Between ->
            Maybe.map2 Criterion.Between
                lowerBound.textField.value
                upperBound.textField.value

        Outside ->
            Maybe.map2 Criterion.Outside
                lowerBound.textField.value
                upperBound.textField.value



---- UPDATE -------------------------------------------------------------------


type Msg
    = SelectQueryType QueryType
    | LowerBoundMsg BoundMsg
    | UpperBoundMsg BoundMsg
    | FocusFirstInput
    | NoOp


type BoundMsg
    = TextFieldMsg TextField.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectQueryType queryType ->
            ( { model | queryType = queryType }
            , (case queryType of
                LessThan ->
                    focusUpperBoundInput

                GreaterThan ->
                    focusLowerBoundInput

                Between ->
                    focusLowerBoundInput

                Outside ->
                    focusLowerBoundInput
              )
                model.elementDetail.id
            )

        LowerBoundMsg lowerBoundMsg ->
            ( { model | lowerBound = updateBound lowerBoundMsg model.lowerBound }
            , Cmd.none
            )

        UpperBoundMsg upperBoundMsg ->
            ( { model | upperBound = updateBound upperBoundMsg model.upperBound }
            , Cmd.none
            )

        FocusFirstInput ->
            case model.queryType of
                LessThan ->
                    ( model, focusUpperBoundInput model.elementDetail.id )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateBound : BoundMsg -> Bound -> Bound
updateBound msg bound =
    case msg of
        TextFieldMsg textFieldMsg ->
            { bound | textField = TextField.update textFieldMsg bound.textField }


focusLowerBoundInput : Urn -> Cmd Msg
focusLowerBoundInput id =
    Dom.focus ("criterion-float-lower-bound-" ++ id)
        |> Task.attempt (\_ -> NoOp)


focusUpperBoundInput : Urn -> Cmd Msg
focusUpperBoundInput id =
    Dom.focus ("criterion-float-upper-bound-" ++ id)
        |> Task.attempt (\_ -> NoOp)



---- VIEW ---------------------------------------------------------------------


type BoundModifier
    = NormalBound
    | OutsideBound


view : Model -> Html Msg
view { elementDetail, queryType, lowerBound, upperBound } =
    let
        lowerBoundInput modifier =
            boundInput elementDetail
                (elementDetail.designation
                    ++ (case modifier of
                            NormalBound ->
                                " should be bigger than this"

                            OutsideBound ->
                                " should be smaller than this"
                       )
                )
                ("criterion-float-lower-bound-" ++ elementDetail.id)
                "Lower Bound"
                lowerBound
                |> Html.map LowerBoundMsg

        upperBoundInput modifier =
            boundInput elementDetail
                (elementDetail.designation
                    ++ (case modifier of
                            NormalBound ->
                                " should be smaller than this"

                            OutsideBound ->
                                " should be bigger than this"
                       )
                )
                ("criterion-float-upper-bound-" ++ elementDetail.id)
                "Upper Bound"
                upperBound
                |> Html.map UpperBoundMsg

        placeholder =
            Html.div [ Attr.class "bound-placeholder" ] []

        inputs =
            case queryType of
                LessThan ->
                    [ upperBoundInput NormalBound, placeholder ]

                GreaterThan ->
                    [ lowerBoundInput NormalBound, placeholder ]

                Between ->
                    [ lowerBoundInput NormalBound
                    , upperBoundInput NormalBound
                    ]

                Outside ->
                    [ lowerBoundInput OutsideBound
                    , upperBoundInput OutsideBound
                    ]
    in
    Dialog.content []
        (description elementDetail :: viewQueryTypeSelector queryType :: inputs)


description elementDetail =
    let
        appendUnit str =
            Maybe.map ((++) (str ++ " in ")) (DataElement.getUnit elementDetail)
                |> Maybe.withDefault str
    in
    Html.p []
        [ Html.text <| appendUnit "Metric item" ++ "." ]


viewQueryTypeSelector : QueryType -> Html Msg
viewQueryTypeSelector currentQueryType =
    let
        button queryType normalLabel phoneLabel =
            Button.view
                [ Options.class normalLabel
                , if queryType == currentQueryType then
                    Button.unelevated

                  else
                    Button.outlined
                , Options.onClick <| SelectQueryType queryType
                ]
                [ Html.span
                    [ Attr.class "text--desktop" ]
                    [ Html.text normalLabel ]
                , Html.span
                    [ Attr.class "text--tablet" ]
                    [ Html.text phoneLabel ]
                , Html.span
                    [ Attr.class "text--phone" ]
                    [ Html.text phoneLabel ]
                ]
    in
    Html.div [ Attr.class "query-type-selector" ]
        [ button LessThan "Less Than" "<"
        , button GreaterThan "Greater Than" ">"
        , button Between "Between" "Between"
        , button Outside "Outside" "Outside"
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
