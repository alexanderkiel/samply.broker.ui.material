port module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Data.Command exposing (SyncToken)
import Data.Name exposing (Name)
import Data.Search as Search
import Dict exposing (Dict)
import EventSub exposing (EventSub)
import Html exposing (Html)
import Html.Attributes exposing (class, href)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Material.Button as Button
import Material.Card as Card
import Material.LayoutGrid as LayoutGrid
import Material.List as List
import Material.Options as Options
import Material.TopAppBar as TopAppBar
import Page.Home
import Page.Search
import Route
import Url exposing (Url)
import Url.Builder
import Util
import WebSocketClient
    exposing
        ( Config
        , PortVersion(..)
        , Response(..)
        , State
        , makeConfig
        , makeState
        )



---- PAGE STUFF ---------------------------------------------------------------


type Page
    = NotFound
    | Home Page.Home.Model
    | Search Page.Search.Model


type PageMsg
    = HomeMsg Page.Home.Msg
    | SearchMsg Page.Search.Msg


initHomePage : Route.Key -> ( Page, Cmd PageMsg )
initHomePage routeKey =
    Page.Home.init routeKey
        |> Tuple.mapBoth Home (Cmd.map HomeMsg)


initSearchPage : InitializedModel -> Search.Id -> ( Page, Cmd PageMsg )
initSearchPage { mdrRoot, mdrNamespace, searchStoreSyncToken } id =
    Page.Search.init { mdrRoot = mdrRoot, mdrNamespace = mdrNamespace }
        searchStoreSyncToken
        id
        |> Tuple.mapBoth Search (Cmd.map SearchMsg)


{-| Updates the model of a single page.

    Only messages matching the page type are considered. Messages from other
    pages are just ignored.

-}
updatePage : PageMsg -> Page -> ( Page, Cmd PageMsg )
updatePage msg model =
    case ( msg, model ) of
        ( HomeMsg homeMsg, Home homeModel ) ->
            Page.Home.update homeMsg homeModel
                |> Tuple.mapBoth Home (Cmd.map HomeMsg)

        ( SearchMsg searchMsg, Search searchModel ) ->
            Page.Search.update searchMsg searchModel
                |> Tuple.mapBoth Search (Cmd.map SearchMsg)

        _ ->
            ( model, Cmd.none )



---- MAIN STUFF ---------------------------------------------------------------


{-| As we read flags at init, the model can be either `Initialized` correctly or
an `InitError`.
-}
type Model
    = Initialized InitializedModel
    | InitError Decode.Error


{-| The correctly initialized model contains top-level configuration values and
the currently active page.
-}
type alias InitializedModel =
    { navKey : Nav.Key
    , mdrRoot : String
    , mdrNamespace : String
    , page : Page
    , searchStoreSyncToken : Maybe SyncToken
    , webSocketState : WebSocketClient.State Msg
    , eventStreamUrl : String
    , eventStreamState : StreamState
    , eventSubs : EventSub.State Msg
    }


type StreamState
    = Open
    | Closed



---- INIT ---------------------------------------------------------------------


type alias Flags =
    { mdrRoot : String
    , mdrNamespace : String
    , protocol : String
    , host : String
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url navKey =
    case Decode.decodeValue flagsDecoder flagsValue of
        Ok flags ->
            { navKey = navKey
            , mdrRoot = flags.mdrRoot
            , mdrNamespace = flags.mdrNamespace
            , page = NotFound
            , searchStoreSyncToken = Nothing
            , webSocketState = initWebSocketState
            , eventStreamUrl =
                Url.Builder.crossOrigin
                    (websocketProtocol flags.protocol ++ "//" ++ flags.host)
                    [ "api", "event-stream" ]
                    []
            , eventStreamState = Closed
            , eventSubs = EventSub.init
            }
                |> wsOpenEventStream
                |> Util.withCmd (routeTo url)
                |> Tuple.mapFirst Initialized

        Err error ->
            ( InitError error, Cmd.none )


websocketProtocol protocol =
    case protocol of
        "http:" ->
            "ws:"

        _ ->
            "wss:"


initWebSocketState =
    WebSocketClient.makeState <| WebSocketClient.makeConfig webSocketClientCmd


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> required "mdrRoot" Decode.string
        |> required "mdrNamespace" Decode.string
        |> required "protocol" Decode.string
        |> required "host" Decode.string



---- UPDATE -------------------------------------------------------------------


type Msg
    = ClickedLink UrlRequest
    | ChangedUrl Url
    | Login
    | PageMsg PageMsg
    | WebSocketReceive Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Initialized initializedModel ->
            updateInitialized msg initializedModel
                |> Tuple.mapFirst Initialized

        InitError _ ->
            ( model, Cmd.none )


updateInitialized : Msg -> InitializedModel -> ( InitializedModel, Cmd Msg )
updateInitialized msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        ChangedUrl url ->
            routeTo url model

        Login ->
            ( model, Nav.load loginUri )

        PageMsg pageMsg ->
            model
                |> updateSearchStoreSyncToken pageMsg
                |> processPageUpdate (updatePage pageMsg model.page)

        WebSocketReceive value ->
            WebSocketClient.process model.webSocketState value
                |> processWebSocketUpdate model


processWebSocketUpdate :
    InitializedModel
    -> ( State Msg, Response Msg )
    -> ( InitializedModel, Cmd Msg )
processWebSocketUpdate model ( state, response ) =
    let
        newModel =
            { model | webSocketState = state }
    in
    case response of
        NoResponse ->
            ( newModel, Cmd.none )

        CmdResponse cmd ->
            ( newModel, cmd )

        ConnectedResponse _ ->
            ( { newModel | eventStreamState = Open, eventSubs = EventSub.init }
            , Cmd.none
            )

        MessageReceivedResponse { key, message } ->
            let
                decodedMsg =
                    Decode.decodeString EventSub.decoder message

                ( newEventSubs, effect ) =
                    case decodedMsg of
                        Ok msg ->
                            EventSub.update (EventSub.InMsg msg) model.eventSubs

                        Err err ->
                            ( model.eventSubs, EventSub.NoEffect )
            in
            ( { newModel | eventSubs = newEventSubs }
            , Cmd.none
            )
                |> Util.withCmd (sendEffect effect)
                |> Util.withCmd (processEffectMsg effect)

        ClosedResponse _ ->
            { newModel | eventStreamState = Closed }
                |> wsOpenEventStream

        ErrorResponse _ ->
            { newModel | eventStreamState = Closed }
                |> wsOpenEventStream


updateSearchStoreSyncToken : PageMsg -> InitializedModel -> InitializedModel
updateSearchStoreSyncToken pageMsg model =
    let
        maybeSyncToken =
            case pageMsg of
                HomeMsg homeMsg ->
                    Page.Home.getSearchStoreSyncToken homeMsg

                _ ->
                    Nothing
    in
    case maybeSyncToken of
        Just syncToken ->
            { model | searchStoreSyncToken = Just syncToken }

        Nothing ->
            model


routeTo : Url -> InitializedModel -> ( InitializedModel, Cmd Msg )
routeTo url model =
    case Route.fromUrl url of
        Ok route ->
            case route of
                Route.Home ->
                    model
                        |> processPageUpdate (initHomePage (Route.Key model.navKey))

                Route.Search id ->
                    model
                        |> processPageUpdate (initSearchPage model id)

        Err _ ->
            ( model, Cmd.none )


processPageUpdate :
    ( Page, Cmd PageMsg )
    -> InitializedModel
    -> ( InitializedModel, Cmd Msg )
processPageUpdate ( page, cmd ) model =
    let
        pageEventSubs =
            pageEventSubscriptions page
                |> EventSub.map PageMsg
                |> EventSub.EventSub

        ( newEventSubs, effect ) =
            EventSub.update pageEventSubs model.eventSubs
    in
    ( { model | page = page, eventSubs = newEventSubs }
    , Cmd.map PageMsg cmd
    )
        |> Util.withCmd (sendEffect effect)
        |> Util.withCmd (processEffectMsg effect)


sendEffect : EventSub.Effect Msg -> InitializedModel -> ( InitializedModel, Cmd Msg )
sendEffect effect model =
    -- only send effects if the stream is open
    if model.eventStreamState == Open then
        case EventSub.encodeEffect effect of
            Just message ->
                wsSendToEventStream message model

            Nothing ->
                ( model, Cmd.none )

    else
        ( model, Cmd.none )


processEffectMsg : EventSub.Effect Msg -> InitializedModel -> ( InitializedModel, Cmd Msg )
processEffectMsg effect model =
    case EventSub.effectMsg effect of
        Just msg ->
            updateInitialized msg model

        Nothing ->
            ( model, Cmd.none )


loginUri : String
loginUri =
    "https://auth.dev.germanbiobanknode.de/grant.xhtml?client_id=1p3id44k4qs09&scope=openid&redirect_uri=http%3A%2F%2Flocalhost%3A8080"



---- VIEW ---------------------------------------------------------------------


view : Model -> Document Msg
view model =
    case model of
        Initialized initializedModel ->
            viewInitialized initializedModel

        InitError error ->
            { title = "Initialization Error"
            , body = [ Html.text "We are sorry. The application isn't initialized correctly. Please try later." ]
            }


viewInitialized : InitializedModel -> Document Msg
viewInitialized model =
    case model.page of
        NotFound ->
            { title = "Not Found"
            , body = [ Html.text "Not Found" ]
            }

        Home page ->
            Page.Home.view page
                |> mapView HomeMsg

        Search page ->
            Page.Search.view page
                |> mapView SearchMsg


mapView : (msg -> PageMsg) -> Document msg -> Document Msg
mapView pageMsg page =
    { title = page.title ++ " - Search Broker"
    , body = List.map (Html.map (pageMsg >> PageMsg)) page.body
    }


loginButton : Html Msg
loginButton =
    Button.view [ Button.outlined, Options.onClick Login ] [ Html.text "Login" ]



---- SUBSCRIPTIONS ------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialized { page } ->
            let
                pageSub =
                    case page of
                        Search pageModel ->
                            Page.Search.subscriptions pageModel
                                |> Sub.map (SearchMsg >> PageMsg)

                        _ ->
                            Sub.none
            in
            Sub.batch [ pageSub, webSocketClientSub WebSocketReceive ]

        InitError _ ->
            Sub.none


pageEventSubscriptions : Page -> EventSub PageMsg
pageEventSubscriptions page =
    case page of
        Search pageModel ->
            Page.Search.eventSubscriptions pageModel
                |> EventSub.map SearchMsg

        _ ->
            EventSub.none



---- WEB SOCKET ---------------------------------------------------------------


wsOpenEventStream : InitializedModel -> ( InitializedModel, Cmd Msg )
wsOpenEventStream model =
    wsOpen model.eventStreamUrl model


wsSendToEventStream : Value -> InitializedModel -> ( InitializedModel, Cmd Msg )
wsSendToEventStream message model =
    wsSend model.eventStreamUrl message model


wsOpen : String -> InitializedModel -> ( InitializedModel, Cmd Msg )
wsOpen url model =
    WebSocketClient.open PortVersion2 model.webSocketState url
        |> processWebSocketUpdate model


wsSend : String -> Value -> InitializedModel -> ( InitializedModel, Cmd Msg )
wsSend url message model =
    WebSocketClient.send PortVersion2
        model.webSocketState
        url
        (Encode.encode 0 message)
        |> processWebSocketUpdate model



---- PORTS --------------------------------------------------------------------


port webSocketClientCmd : Value -> Cmd msg


port webSocketClientSub : (Value -> msg) -> Sub msg



---- MAIN ---------------------------------------------------------------------


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
