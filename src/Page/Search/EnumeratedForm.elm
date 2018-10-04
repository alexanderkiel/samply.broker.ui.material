module Page.Search.EnumeratedForm exposing (Model, Msg, init, update, view)

import Data.Mdr.DataElement as DataElement exposing (DataElementDetail)
import Data.Search.Criterion as Criterion exposing (Criterion)
import Html exposing (Html)
import Html.Attributes as Attr
import Material.Checkbox as Checkbox
import Material.Dialog as Dialog
import Material.FormField as FormField
import Material.Options as Options


type alias Model =
    { elementDetail : DataElementDetail
    , valueSelects : List ValueSelect
    }


type alias ValueSelect =
    { value : String
    , designation : String
    , selected : Bool
    }


init : DataElementDetail -> List String -> Model
init elementDetail selectedValues =
    case elementDetail.validation of
        DataElement.Enumerated values ->
            { elementDetail = elementDetail
            , valueSelects = List.map (initValueSelect selectedValues) values
            }

        _ ->
            { elementDetail = elementDetail
            , valueSelects = List.map initValueSelectWithoutMetadata selectedValues
            }


initValueSelect : List String -> DataElement.Value -> ValueSelect
initValueSelect selectedValues value =
    { value = value.value
    , designation = value.designation
    , selected = List.member value.value selectedValues
    }


initValueSelectWithoutMetadata : String -> ValueSelect
initValueSelectWithoutMetadata value =
    { value = value
    , designation = value
    , selected = True
    }



---- UPDATE -------------------------------------------------------------------


type Msg
    = ToggleSelection String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleSelection selectedValue ->
            { model | valueSelects = toggleSelection selectedValue model.valueSelects }


toggleSelection : String -> List ValueSelect -> List ValueSelect
toggleSelection selectedValue =
    List.map <|
        \value ->
            if selectedValue == value.value then
                { value | selected = not value.selected }

            else
                value



---- VIEW ---------------------------------------------------------------------


view : Model -> Html Msg
view { valueSelects } =
    Dialog.content [] <|
        List.map enumeratedValue valueSelects


enumeratedValue : ValueSelect -> Html Msg
enumeratedValue { value, designation, selected } =
    Html.div [ Attr.class "form-field-container" ]
        [ FormField.view
            [ Options.onClick <| ToggleSelection value ]
            [ Checkbox.view [ Checkbox.checked <| Just selected ] []
            , Html.label [] [ Html.text designation ]
            ]
        ]
