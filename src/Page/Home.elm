module Page.Home exposing
    ( Model
    , Msg(..)
    , appBar
    , init
    , startNewSearchButton
    , update
    , view
    , welcomePanel
    )

import Browser exposing (Document)
import Browser.Navigation as Nav
import Data.Command exposing (emptyCommand)
import Data.Search as Search
import Html exposing (Html)
import Html.Attributes as Attr
import Material.Button as Button
import Material.Options as Options
import Material.TopAppBar as TopAppBar
import Ports
import Request.Command exposing (CreateCommandResult)
import Request.Error as Request
import Route
import Task



---- MODEL --------------------------------------------------------------------


type alias Model =
    { routeKey : Route.Key
    , searchStarted : Bool
    }


init : Route.Key -> ( Model, Cmd Msg )
init routeKey =
    ( { routeKey = routeKey, searchStarted = False }
    , Cmd.none
    )



---- UPDATE -------------------------------------------------------------------


type Msg
    = StartNewSearch
    | SearchCreated (Result Request.Error CreateCommandResult)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartNewSearch ->
            ( { model | searchStarted = True }
            , emptyCommand "search" "create"
                |> Request.Command.performCreate
                |> Task.attempt SearchCreated
            )

        SearchCreated result ->
            case result of
                Ok { id } ->
                    ( model, Route.goTo model.routeKey <| Route.Search id )

                Err _ ->
                    ( model, Cmd.none )



---- VIEW ---------------------------------------------------------------------


view : Model -> Document Msg
view model =
    { title = "Home"
    , body =
        [ appBar
        , Html.div [ Attr.class "mdc-top-app-bar--fixed-adjust" ]
            [ welcomePanel ]
        ]
    }


appBar : Html msg
appBar =
    TopAppBar.view []
        [ TopAppBar.section [ TopAppBar.alignStart ]
            [ TopAppBar.navigationIcon "menu"
            , TopAppBar.title "Search Broker"
            ]
        ]


welcomePanel : Html Msg
welcomePanel =
    Html.div [ Attr.class "mdc-top-app-bar--fixed-adjust" ]
        [ Html.div [ Attr.class "welcome-panel" ] [ startNewSearchButton ] ]


startNewSearchButton : Html Msg
startNewSearchButton =
    Button.view [ Button.outlined, Options.onClick StartNewSearch ]
        [ Html.text "Start New Search" ]
