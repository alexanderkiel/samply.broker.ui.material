module Page.Search.CriterionForm exposing
    ( Model
    , Msg
    , focusFirstInput
    , getElementDetail
    , init
    , toCriterion
    , update
    , view
    )

import Data.Mdr.DataElement as DataElement exposing (DataElementDetail)
import Data.Search.Criterion as Criterion exposing (Criterion)
import Html exposing (Html)
import Html.Attributes as Attr
import Material.Dialog as Dialog
import Page.Search.EnumeratedForm as EnumeratedForm
import Page.Search.FloatForm as FloatForm
import Time exposing (Posix)



---- MODEL --------------------------------------------------------------------


type Model
    = Enumerated EnumeratedForm.Model
    | Date DateForm
    | Float FloatForm.Model


type alias DateForm =
    { elementDetail : DataElementDetail
    , start : Maybe Posix
    , end : Maybe Posix
    }


init : DataElementDetail -> Maybe Criterion.Query -> Model
init elementDetail maybeQuery =
    case maybeQuery of
        Just query ->
            initInEditMode elementDetail query

        Nothing ->
            initInCreateMode elementDetail


initInCreateMode : DataElementDetail -> Model
initInCreateMode elementDetail =
    case elementDetail.validation of
        DataElement.Enumerated _ ->
            Enumerated <| EnumeratedForm.init elementDetail []

        DataElement.Date ->
            Date
                { elementDetail = elementDetail
                , start = Nothing
                , end = Nothing
                }

        DataElement.Float _ ->
            Float <| FloatForm.init elementDetail Nothing


initInEditMode : DataElementDetail -> Criterion.Query -> Model
initInEditMode elementDetail query =
    case query of
        Criterion.Enumerated selectedValues ->
            Enumerated <| EnumeratedForm.init elementDetail selectedValues

        Criterion.Date start end ->
            Date
                { elementDetail = elementDetail
                , start = start
                , end = end
                }

        Criterion.Float metricQuery ->
            Float <| FloatForm.init elementDetail <| Just metricQuery


getElementDetail : Model -> DataElementDetail
getElementDetail model =
    case model of
        Enumerated { elementDetail } ->
            elementDetail

        Date { elementDetail } ->
            elementDetail

        Float { elementDetail } ->
            elementDetail


toCriterion : Model -> Maybe Criterion
toCriterion form =
    case form of
        Enumerated { elementDetail, valueSelects } ->
            let
                values =
                    valueSelects
                        |> List.filter .selected
                        |> List.map .value
            in
            if List.isEmpty values then
                Nothing

            else
                Just
                    { mdrKey = elementDetail.id
                    , query = Criterion.Enumerated values
                    }

        Date { elementDetail, start, end } ->
            Just
                { mdrKey = elementDetail.id
                , query = Criterion.Date start end
                }

        Float ({ elementDetail } as floatForm) ->
            FloatForm.toMetricQuery floatForm
                |> Maybe.map
                    (\metricQuery ->
                        { mdrKey = elementDetail.id
                        , query = Criterion.Float metricQuery
                        }
                    )



---- UPDATE -------------------------------------------------------------------


type Msg
    = EnumeratedFormMsg EnumeratedForm.Msg
    | DateFormMsg DateMsg
    | FloatFormMsg FloatForm.Msg


type DateMsg
    = StartInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( EnumeratedFormMsg enumeratedMsg, Enumerated enumeratedForm ) ->
            EnumeratedForm.update enumeratedMsg enumeratedForm
                |> Tuple.mapBoth Enumerated (Cmd.map EnumeratedFormMsg)

        ( DateFormMsg dateMsg, Date dateForm ) ->
            updateDateForm dateMsg dateForm
                |> Tuple.mapBoth Date (Cmd.map DateFormMsg)

        ( FloatFormMsg floatMsg, Float floatForm ) ->
            FloatForm.update floatMsg floatForm
                |> Tuple.mapBoth Float (Cmd.map FloatFormMsg)

        _ ->
            ( model, Cmd.none )


updateDateForm : DateMsg -> DateForm -> ( DateForm, Cmd DateMsg )
updateDateForm msg model =
    case msg of
        StartInput _ ->
            ( model, Cmd.none )


focusFirstInput : Model -> ( Model, Cmd Msg )
focusFirstInput model =
    case model of
        Float floatForm ->
            FloatForm.update FloatForm.FocusFirstInput floatForm
                |> Tuple.mapBoth Float (Cmd.map FloatFormMsg)

        _ ->
            ( model, Cmd.none )



---- VIEW ---------------------------------------------------------------------


view : Model -> Html Msg
view model =
    case model of
        Enumerated enumeratedForm ->
            EnumeratedForm.view enumeratedForm |> Html.map EnumeratedFormMsg

        Date dateForm ->
            dateBody dateForm

        Float floatForm ->
            FloatForm.view floatForm |> Html.map FloatFormMsg


dateBody _ =
    Dialog.content [] []
