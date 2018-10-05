module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Data.Command exposing (SyncToken)
import Data.Search as Search
import Html exposing (Html)
import Html.Attributes exposing (class, href)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Material.Button as Button
import Material.Card as Card
import Material.LayoutGrid as LayoutGrid
import Material.List as List
import Material.Options as Options
import Material.TopAppBar as TopAppBar
import Page.Home
import Page.Search
import Ports
import Route
import Url exposing (Url)



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
    }



---- INIT ---------------------------------------------------------------------


type alias Flags =
    { mdrRoot : String
    , mdrNamespace : String
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url navKey =
    case Decode.decodeValue flagsDecoder flagsValue of
        Ok flags ->
            routeTo url
                { navKey = navKey
                , mdrRoot = flags.mdrRoot
                , mdrNamespace = flags.mdrNamespace
                , page = NotFound
                , searchStoreSyncToken = Nothing
                }
                |> Tuple.mapFirst Initialized

        Err error ->
            ( InitError error, Cmd.none )


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> required "mdrRoot" Decode.string
        |> required "mdrNamespace" Decode.string



---- UPDATE -------------------------------------------------------------------


type Msg
    = ClickedLink UrlRequest
    | ChangedUrl Url
    | Login
    | PageMsg PageMsg
    | Receive (Result Decode.Error Ports.InMsg)


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
                |> pageUpdate (updatePage pageMsg model.page)

        Receive result ->
            case result of
                Ok inMsg ->
                    receive inMsg model

                Err _ ->
                    ( model, Cmd.none )


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
                        |> pageUpdate (initHomePage (Route.Key model.navKey))

                Route.Search id ->
                    model
                        |> pageUpdate (initSearchPage model id)

        Err _ ->
            ( model, Cmd.none )


pageUpdate : ( Page, Cmd PageMsg ) -> InitializedModel -> ( InitializedModel, Cmd Msg )
pageUpdate ( page, cmd ) model =
    ( { model | page = page }
    , Cmd.map PageMsg cmd
    )


receive : Ports.InMsg -> InitializedModel -> ( InitializedModel, Cmd Msg )
receive msg model =
    case ( msg, model.page ) of
        _ ->
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
            case page of
                Search pageModel ->
                    Page.Search.subscriptions pageModel
                        |> Sub.map (SearchMsg >> PageMsg)

                _ ->
                    Sub.none

        InitError _ ->
            Sub.none



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
