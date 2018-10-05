module Page.Home exposing
    ( Model
    , init
    , Msg
    , update
    , getSearchStoreSyncToken
    , view
    )

{-| The home page displays a button creating new searches.


# Model

@docs Model
@docs init


# Update

@docs Msg
@docs update
@docs getSearchStoreSyncToken


# View

@docs view

-}

import Browser exposing (Document)
import Browser.Navigation as Nav
import Data.Command as Command exposing (SyncToken, emptyCommand)
import Data.Search as Search
import Html exposing (Html)
import Html.Attributes as Attr
import Material.Button as Button
import Material.Options as Options
import Material.TopAppBar as TopAppBar
import Ports
import Request.Command
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
    ( { routeKey = routeKey
      , searchStarted = False
      }
    , Cmd.none
    )



---- UPDATE -------------------------------------------------------------------


type Msg
    = StartNewSearch
    | SearchCreated (Result Request.Error Command.CreateResult)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartNewSearch ->
            ( { model | searchStarted = True }
            , emptyCommand (Command.Name "search" "create")
                |> Request.Command.performCreate
                |> Task.attempt SearchCreated
            )

        SearchCreated result ->
            case result of
                Ok { id } ->
                    ( model, Route.goTo model.routeKey <| Route.Search id )

                Err _ ->
                    ( model, Cmd.none )


{-| Extracts optional sync tokens from the home pages messages.

    The home page causes effects in the search store. Messages returned by such
    effects carry sync tokens. Such sync tokens are important for other pages.

-}
getSearchStoreSyncToken : Msg -> Maybe SyncToken
getSearchStoreSyncToken msg =
    case msg of
        SearchCreated (Ok { t }) ->
            Just t

        _ ->
            Nothing



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
