module Route exposing (Key(..), Route(..), fromUrl, goTo)

import Browser.Navigation as Nav
import Data.Search as Search
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, parse, s, string, top)


type Key
    = Key Nav.Key


type Route
    = Home
    | Search Search.Id


fromUrl : Url -> Result Url Route
fromUrl url =
    case parse routeParser url of
        Just route ->
            Ok route

        Nothing ->
            Err url


goTo : Key -> Route -> Cmd msg
goTo (Key navKey) route =
    Nav.pushUrl navKey <| routeToString route



---- INTERNAL -----------------------------------------------------------------


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Search (s "search" </> string)
        ]


routeToString : Route -> String
routeToString route =
    let
        pathSegments =
            case route of
                Home ->
                    []

                Search id ->
                    [ "search", id ]
    in
    Builder.absolute pathSegments []
