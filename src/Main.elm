module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
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
    let
        ( model, cmd ) =
            Page.Home.init routeKey
    in
    ( Home model, Cmd.map HomeMsg cmd )


initSearchPage : String -> Search.Id -> ( Page, Cmd PageMsg )
initSearchPage mdrRoot id =
    let
        ( model, cmd ) =
            Page.Search.init mdrRoot id
    in
    ( Search model, Cmd.map SearchMsg cmd )


updatePage : PageMsg -> Page -> ( Page, Cmd PageMsg )
updatePage msg model =
    case ( msg, model ) of
        ( HomeMsg homeMsg, Home homeModel ) ->
            updatePage2 Page.Home.update HomeMsg Home homeMsg homeModel

        ( SearchMsg searchMsg, Search searchModel ) ->
            updatePage2 Page.Search.update SearchMsg Search searchMsg searchModel

        _ ->
            ( model, Cmd.none )


updatePage2 :
    (msg -> model -> ( model, Cmd msg ))
    -> (msg -> PageMsg)
    -> (model -> Page)
    -> msg
    -> model
    -> ( Page, Cmd PageMsg )
updatePage2 updateFn toPageMsg toPage msg model =
    let
        ( newModel, cmd ) =
            updateFn msg model
    in
    ( toPage newModel, Cmd.map toPageMsg cmd )



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
    , page : Page
    }



---- INIT ---------------------------------------------------------------------


type alias Flags =
    { mdrRoot : String }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url navKey =
    case Decode.decodeValue flagsDecoder flagsValue of
        Ok flags ->
            routeTo url
                { navKey = navKey
                , mdrRoot = flags.mdrRoot
                , page = NotFound
                }
                |> Tuple.mapFirst Initialized

        Err error ->
            ( InitError error, Cmd.none )


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> required "mdrRoot" Decode.string



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
            updatePage pageMsg model.page
                |> pageUpdate model

        Receive result ->
            case result of
                Ok inMsg ->
                    receive inMsg model

                Err _ ->
                    ( model, Cmd.none )


routeTo : Url -> InitializedModel -> ( InitializedModel, Cmd Msg )
routeTo url model =
    case Route.fromUrl url of
        Ok route ->
            case route of
                Route.Home ->
                    initHomePage (Route.Key model.navKey)
                        |> pageUpdate model

                Route.Search id ->
                    initSearchPage model.mdrRoot id
                        |> pageUpdate model

        Err _ ->
            ( model, Cmd.none )


pageUpdate : InitializedModel -> ( Page, Cmd PageMsg ) -> ( InitializedModel, Cmd Msg )
pageUpdate model ( page, cmd ) =
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
