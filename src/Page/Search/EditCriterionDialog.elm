module Page.Search.EditCriterionDialog exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Data.Search.Criterion exposing (Criterion)
import Html exposing (Html)
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Options as Options
import Page.Search.CriterionForm as CriterionForm


type alias Model =
    CriterionForm.Model


init =
    CriterionForm.init



---- UPDATE -------------------------------------------------------------------


type alias Msg =
    CriterionForm.Msg


update : Msg -> Model -> Model
update =
    CriterionForm.update



---- VIEW ---------------------------------------------------------------------


type alias Config msg =
    { onOk : Criterion -> msg
    , onCancel : msg
    , map : Msg -> msg
    }


view : Config msg -> Model -> Bool -> Bool -> Html msg
view config model open okButtonDisabled =
    let
        criterion =
            CriterionForm.toCriterion model
    in
    Dialog.view [ Options.when open Dialog.open ]
        [ Dialog.container []
            [ Dialog.surface [ Options.class "mdc-dialog__surface--constant-width" ]
                [ Options.styled Html.h2
                    [ Dialog.title ]
                    [ Html.text <| "Criterion " ++ (CriterionForm.getElementDetail model).designation ]
                , CriterionForm.view model
                    |> Html.map config.map
                , Dialog.actions
                    [ Button.view [ Dialog.button, Options.onClick config.onCancel ]
                        [ Html.text "Cancel" ]
                    , Button.view
                        [ Dialog.button
                        , Button.unelevated
                        , Maybe.map (Options.onClick << config.onOk) criterion
                            |> Maybe.withDefault Options.noOp
                        , Button.disabled <| okButtonDisabled || criterion == Nothing
                        ]
                        [ Html.text "Ok" ]
                    ]
                ]
            ]
        , Dialog.scrim [] []
        ]
