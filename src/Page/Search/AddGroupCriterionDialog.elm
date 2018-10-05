module Page.Search.AddGroupCriterionDialog exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

{-| This module contains a dialog for choosing one data element out of a list
of data elements out of a data element group.

    The dialog maintains internal state, so one has to call the `update`
    function with messages supplied though the function of the `map` record
    of the config.

-}

import Data.LoadingStatus as LoadingStatus exposing (LoadingStatus(..))
import Data.Mdr.DataElement exposing (DataElement, DataElementDetail)
import Data.Mdr.DataElementGroup exposing (DataElementGroup)
import Data.Search.Criterion as Criterion exposing (Criterion)
import Data.Urn exposing (Urn)
import Html exposing (Html)
import Html.Attributes as Attr
import Material.Button as Button
import Material.CircularProgress as CircularProgress
import Material.Dialog as Dialog
import Material.FormField as FormField
import Material.List as List
import Material.Options as Options
import Material.RadioButton as RadioButton
import Page.Search.CriterionForm as CriterionForm
import Request.Error as Request
import Request.Mdr
import Task


type Model
    = SelectItem
        { mdrRoot : String
        , usedMdrKeys : List Urn
        , selectedItemId : Maybe Urn
        }
    | LoadingDataElementDetail Urn
    | LoadingDataElementDetailSlowly Urn
    | CreateCriterion CriterionForm.Model


{-| Initializes the model.

    The data elements with identifiers contained in `usedMdrKeys` will be
    disabled, so that the user can't select one data element twice.

-}
init : String -> List Urn -> Model
init mdrRoot usedMdrKeys =
    SelectItem
        { mdrRoot = mdrRoot
        , usedMdrKeys = usedMdrKeys
        , selectedItemId = Nothing
        }



---- UPDATE -------------------------------------------------------------------


type Msg
    = SelectElement Urn
    | Next Urn
    | DataElementDetailLoaded Urn (Result Request.Error DataElementDetail)
    | PassedSlowLoadThreshold
    | CommonMsg CriterionForm.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectElement id ->
            case model of
                SelectItem selectItemModel ->
                    ( SelectItem { selectItemModel | selectedItemId = Just id }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Next selectedItemId ->
            case model of
                SelectItem { mdrRoot } ->
                    ( LoadingDataElementDetail selectedItemId
                    , Cmd.batch
                        [ loadElementDetail mdrRoot selectedItemId
                        , Task.perform (\_ -> PassedSlowLoadThreshold)
                            LoadingStatus.slowThreshold
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        DataElementDetailLoaded elementId result ->
            case result of
                Ok element ->
                    ( CreateCriterion <| CriterionForm.init element Nothing
                    , Cmd.none
                    )

                Err error ->
                    ( model
                    , Cmd.none
                    )

        PassedSlowLoadThreshold ->
            ( case model of
                LoadingDataElementDetail selectedItemId ->
                    LoadingDataElementDetailSlowly selectedItemId

                _ ->
                    model
            , Cmd.none
            )

        CommonMsg commonMsg ->
            case model of
                CreateCriterion commonModel ->
                    ( CreateCriterion <| CriterionForm.update commonMsg commonModel
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


loadElementDetail : String -> Urn -> Cmd Msg
loadElementDetail mdrRoot elementId =
    Request.Mdr.dataElement mdrRoot elementId
        |> Task.attempt (DataElementDetailLoaded elementId)



---- VIEW ---------------------------------------------------------------------


{-| Configuration of the view layer.

    onOk - a message sended at the user clicking on the Ok button,
           taking the created criterion

    onCancel - a message sended at the user clicking on the Cancel button

    map - a message taking an internal message which has to be supplied to the
          `update` function

-}
type alias Config msg =
    { onOk : DataElementDetail -> Criterion -> msg
    , onCancel : msg
    , map : Msg -> msg
    }


view :
    Config msg
    -> Model
    -> DataElementGroup
    -> LoadingStatus (List DataElement)
    -> Bool
    -> Bool
    -> Html msg
view { onOk, onCancel, map } model { id, designation } loadingMembers open okButtonDisabled =
    let
        title =
            case model of
                CreateCriterion form ->
                    "Create " ++ (CriterionForm.getElementDetail form).designation ++ " Criterion"

                _ ->
                    "Choose " ++ designation ++ " Criterion"

        scrollable =
            case model of
                CreateCriterion form ->
                    Options.noOp

                _ ->
                    Dialog.scrollable
    in
    Dialog.view [ Options.when open Dialog.open, scrollable ]
        [ Dialog.container []
            [ Dialog.surface [ Options.class "mdc-dialog__surface--constant-width" ]
                [ Options.styled Html.h2
                    [ Dialog.title ]
                    [ Html.text title ]
                , body model loadingMembers
                    |> Html.map map
                , Dialog.actions
                    [ Button.view [ Dialog.button, Options.onClick onCancel ]
                        [ Html.text "Cancel" ]
                    , case model of
                        SelectItem { selectedItemId } ->
                            nextButton selectedItemId |> Html.map map

                        LoadingDataElementDetail _ ->
                            waitingNextButton False

                        LoadingDataElementDetailSlowly _ ->
                            waitingNextButton True

                        CreateCriterion form ->
                            okButton (onOk (CriterionForm.getElementDetail form))
                                (CriterionForm.toCriterion form)
                                okButtonDisabled
                    ]
                ]
            ]
        , Dialog.scrim [] []
        ]


nextButton selectedItemId =
    Button.view
        [ Dialog.button
        , Button.unelevated
        , Maybe.map (Options.onClick << Next) selectedItemId
            |> Maybe.withDefault Options.noOp
        , Button.disabled <| selectedItemId == Nothing
        , primaryButtonWidth
        ]
        [ Html.text "Next" ]


waitingNextButton isSlowLoading =
    Button.view
        [ Dialog.button, Button.unelevated, primaryButtonWidth ]
        [ if isSlowLoading then
            CircularProgress.view [] []

          else
            Html.text "Next"
        ]


primaryButtonWidth =
    Options.style "width" "5rem"


okButton onOk criterion okButtonDisabled =
    Button.view
        [ Dialog.button
        , Button.unelevated
        , Maybe.map (Options.onClick << onOk) criterion
            |> Maybe.withDefault Options.noOp
        , Button.disabled <| okButtonDisabled || criterion == Nothing
        , primaryButtonWidth
        ]
        [ Html.text "Ok" ]


body model loadingMembers =
    case model of
        SelectItem { usedMdrKeys, selectedItemId } ->
            selectItemBody usedMdrKeys selectedItemId False loadingMembers

        LoadingDataElementDetail selectedItemId ->
            selectItemBody [] (Just selectedItemId) True loadingMembers

        LoadingDataElementDetailSlowly selectedItemId ->
            selectItemBody [] (Just selectedItemId) True loadingMembers

        CreateCriterion form ->
            CriterionForm.view form |> Html.map CommonMsg


selectItemBody : List Urn -> Maybe Urn -> Bool -> LoadingStatus (List DataElement) -> Html Msg
selectItemBody usedMdrKeys selectedItemId disabled loadingMembers =
    Dialog.content [] <|
        case loadingMembers of
            Loaded elements ->
                List.map (elementItem usedMdrKeys selectedItemId disabled) elements

            _ ->
                [ Html.text "" ]


elementItem : List Urn -> Maybe Urn -> Bool -> DataElement -> Html Msg
elementItem usedMdrKeys selectedItemId allDisabled { id, designation } =
    let
        disabled =
            allDisabled || List.member id usedMdrKeys
    in
    Html.div [ Attr.class "form-field-container" ]
        [ FormField.view
            [ Options.when (not disabled) <| Options.onClick <| SelectElement id
            ]
            [ RadioButton.view
                [ RadioButton.checked <| Just id == selectedItemId
                , RadioButton.disabled disabled
                ]
                []
            , Options.styled Html.label
                [ Options.when disabled <| Options.class "label-disabled" ]
                [ Html.text designation ]
            ]
        ]
