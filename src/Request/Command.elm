module Request.Command exposing
    ( CommandResult
    , CreateCommandResult
    , perform
    , performCreate
    )

import Data.Command as Command exposing (Command)
import Dict exposing (Dict)
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Request.Error exposing (Error, convertError)
import Task exposing (Task)
import Url.Builder as Builder


{-| The result of a command which does an arbitrary side effect.

    See `CreateCommandResult` for the result of a command which created an
    entity.

    The `t` value can be used to fetch at least a state which includes the
    effects of this command.

-}
type alias CommandResult =
    { t : Int }


{-| The result of a command which created an entity.

    The `t` value can be used to fetch at least a state which includes the
    effects of this command.

    The `id` is the identifier of the entity created.

-}
type alias CreateCommandResult =
    { t : Int
    , id : String
    }


{-| Performs (executes) the command.
-}
perform : Command -> Task Error CommandResult
perform command =
    url command
        |> HttpBuilder.post
        |> HttpBuilder.withJsonBody command.params
        |> HttpBuilder.withExpectJson commandResultDecoder
        |> HttpBuilder.toTask
        |> Task.onError convertError


{-| Performs (executes) the command returning a result including the identifier
of the entity created.
-}
performCreate : Command -> Task Error CreateCommandResult
performCreate command =
    url command
        |> HttpBuilder.post
        |> HttpBuilder.withJsonBody command.params
        |> HttpBuilder.withExpectJson createCommandResultDecoder
        |> HttpBuilder.toTask
        |> Task.onError convertError


url : Command -> String
url { namespace, name } =
    Builder.absolute [ "api", "command", namespace, name ] []


commandResultDecoder : Decoder CommandResult
commandResultDecoder =
    Decode.succeed CommandResult
        |> required "t" Decode.int


createCommandResultDecoder : Decoder CreateCommandResult
createCommandResultDecoder =
    Decode.succeed CreateCommandResult
        |> required "t" Decode.int
        |> required "id" Decode.string
