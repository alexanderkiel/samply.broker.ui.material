module Request.Command exposing
    ( perform
    , performCreate
    )

import Data.Command as Command exposing (Command)
import Data.Name exposing (Name(..))
import Dict exposing (Dict)
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Request.Error exposing (Error, convertError)
import Task exposing (Task)
import Url.Builder as Builder


{-| Performs (executes) the command.
-}
perform : Command -> Task Error Command.Result
perform command =
    url command.name
        |> HttpBuilder.post
        |> HttpBuilder.withJsonBody command.params
        |> HttpBuilder.withExpectJson commandResultDecoder
        |> HttpBuilder.toTask
        |> Task.onError convertError


{-| Performs (executes) the command returning a result including the identifier
of the entity created.
-}
performCreate : Command -> Task Error Command.CreateResult
performCreate command =
    url command.name
        |> HttpBuilder.post
        |> HttpBuilder.withJsonBody command.params
        |> HttpBuilder.withExpectJson createCommandResultDecoder
        |> HttpBuilder.toTask
        |> Task.onError convertError


url : Name -> String
url (Name namespace name) =
    Builder.absolute [ "api", "command", namespace, name ] []


commandResultDecoder : Decoder Command.Result
commandResultDecoder =
    Decode.succeed Command.Result
        |> required "t" Command.syncTokenDecoder


createCommandResultDecoder : Decoder Command.CreateResult
createCommandResultDecoder =
    Decode.succeed Command.CreateResult
        |> required "t" Command.syncTokenDecoder
        |> required "id" Decode.string
