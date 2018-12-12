module Page.Search.AddGroupCriterionDialog exposing
    ( Model(..)
    , init
    , Msg(..)
    , update
    , view
    )

{-| This module contains a dialog for choosing one data element out of a list
of data elements out of a data element group.

The dialog maintains internal state, so one has to call the `update`
function with messages supplied though the function of the `map` record
of the config.


# Model

@docs Model
@docs init


# Update

@docs Msg
@docs update


# View

@docs view

-}

import Browser.Dom as Dom
import Data.LoadingStatus as LoadingStatus exposing (LoadingStatus(..))
import Data.Mdr.DataElement exposing (DataElement, DataElementDetail)
import Data.Mdr.DataElementGroup exposing (DataElementGroup)
import Data.Search.Criterion as Criterion exposing (Criterion)
import Data.Urn exposing (Urn)
import Html exposing (Html)
import Html.Attributes as Attr
import List.Selection as Selection
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
        , selection : Selection
        }
    | LoadingDataElementDetail Selection
    | LoadingDataElementDetailSlowly Selection
    | CreateCriterion CriterionForm.Model


type alias Selection =
    Selection.Selection ( DataElement, Bool )


{-| Initializes the model.

    The data elements with identifiers contained in `usedMemberIds` will be
    disabled, so that the user can't select one data element twice.

-}
init : String -> List Urn -> List DataElement -> Model
init mdrRoot usedMemberIds members =
    let
        selection =
            members
                |> tagMembers usedMemberIds
                |> Selection.fromList
                |> Selection.selectBy Tuple.second
    in
    SelectItem
        { mdrRoot = mdrRoot
        , selection = selection
        }


tagMember : List Urn -> DataElement -> ( DataElement, Bool )
tagMember usedMdrKeys member =
    ( member, List.member member.id usedMdrKeys |> not )


tagMembers : List Urn -> List DataElement -> List ( DataElement, Bool )
tagMembers usedMdrKeys =
    List.map (tagMember usedMdrKeys)



---- UPDATE -------------------------------------------------------------------


type Msg
    = SelectElement Urn
    | Next
    | DataElementDetailLoaded Urn (Result Request.Error DataElementDetail)
    | PassedSlowLoadThreshold
    | CommonMsg CriterionForm.Msg
    | FocusSelectedMember
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectElement id ->
            case model of
                SelectItem ({ selection } as selectItemModel) ->
                    let
                        newSelection =
                            Selection.selectBy
                                (\( member, selectable ) ->
                                    id == member.id && selectable
                                )
                                selection
                    in
                    ( SelectItem { selectItemModel | selection = newSelection }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Next ->
            case model of
                SelectItem { mdrRoot, selection } ->
                    case getSelectedMember selection of
                        Just selectedMember ->
                            ( LoadingDataElementDetail selection
                            , Cmd.batch
                                [ loadElementDetail mdrRoot selectedMember.id
                                , Task.perform (\_ -> PassedSlowLoadThreshold)
                                    LoadingStatus.slowThreshold
                                ]
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DataElementDetailLoaded elementId result ->
            case result of
                Ok element ->
                    CriterionForm.init element Nothing
                        |> CriterionForm.focusFirstInput
                        |> Tuple.mapBoth CreateCriterion (Cmd.map CommonMsg)

                Err error ->
                    ( model
                    , Cmd.none
                    )

        PassedSlowLoadThreshold ->
            ( case model of
                LoadingDataElementDetail selection ->
                    LoadingDataElementDetailSlowly selection

                _ ->
                    model
            , Cmd.none
            )

        CommonMsg commonMsg ->
            case model of
                CreateCriterion commonModel ->
                    CriterionForm.update commonMsg commonModel
                        |> Tuple.mapBoth CreateCriterion (Cmd.map CommonMsg)

                _ ->
                    ( model, Cmd.none )

        FocusSelectedMember ->
            case model of
                SelectItem { selection } ->
                    case getSelectedMember selection of
                        Just selectedMember ->
                            ( model
                            , Dom.focus (htmlMemberId selectedMember.id)
                                |> Task.attempt (\_ -> NoOp)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
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
    -> Bool
    -> Bool
    -> Html msg
view { onOk, onCancel, map } model { id, designation } open okButtonDisabled =
    let
        title =
            case model of
                CreateCriterion form ->
                    "Create " ++ (CriterionForm.getElementDetail form).designation ++ " Criterion"

                _ ->
                    "Choose " ++ designation ++ " Criterion"
    in
    Dialog.view [ Options.when open Dialog.open ]
        [ Dialog.container []
            [ Dialog.surface
                [ Options.class "mdc-dialog__surface--constant-width" ]
                [ Options.styled Html.h2
                    [ Dialog.title ]
                    [ Html.text title ]
                , body model id
                    |> Html.map map
                , Dialog.actions
                    [ Button.view [ Dialog.button, Options.onClick onCancel ]
                        [ Html.text "Cancel" ]
                    , case model of
                        SelectItem { selection } ->
                            nextButton (getSelectedMember selection) |> Html.map map

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


getSelectedMember : Selection -> Maybe DataElement
getSelectedMember selection =
    Selection.selected selection
        |> Maybe.map Tuple.first


nextButton selectedMember =
    Button.view
        [ Dialog.button
        , Button.unelevated
        , Options.onClick Next
        , Button.disabled (selectedMember == Nothing)
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


body : Model -> Urn -> Html Msg
body model groupId =
    case model of
        SelectItem { selection } ->
            selectItemBody False selection

        LoadingDataElementDetail selection ->
            selectItemBody True selection

        LoadingDataElementDetailSlowly selection ->
            selectItemBody True selection

        CreateCriterion form ->
            CriterionForm.view form |> Html.map CommonMsg


selectItemBody : Bool -> Selection -> Html Msg
selectItemBody allDisabled selection =
    Dialog.content [] <|
        markSelected (elementItem allDisabled) selection


markSelected : (Bool -> a -> b) -> Selection.Selection a -> List b
markSelected f =
    Selection.mapSelected { selected = f True, rest = f False }
        >> Selection.toList


elementItem : Bool -> Bool -> ( DataElement, Bool ) -> Html Msg
elementItem allDisabled selected ( { id, designation }, selectable ) =
    let
        disabled =
            allDisabled || not selectable
    in
    Html.div [ Attr.class "form-field-container" ]
        [ FormField.view
            [ Options.onClick (SelectElement id)
            ]
            [ RadioButton.view
                [ Options.id (htmlMemberId id)
                , RadioButton.checked selected
                , RadioButton.disabled disabled
                ]
                []
            , Options.styled Html.label
                [ Options.class "label-disabled" |> Options.when disabled ]
                [ Html.text designation ]
            ]
        ]


htmlMemberId id =
    "add-group-criterion-dialog-member-" ++ id
