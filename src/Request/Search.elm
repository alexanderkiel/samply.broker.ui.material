module Request.Search exposing (search)

import Data.Command exposing (SyncToken, fromSyncToken)
import Data.Search exposing (Id, Search, decoder)
import HttpBuilder
import Request.Error exposing (Error, convertError)
import Task exposing (Task)
import Url.Builder as Builder


{-| Returns a task loading the search with the given identifier.
-}
search : Maybe SyncToken -> Id -> Task Error Search
search syncToken id =
    Builder.absolute [ "api", "searches", id ] []
        |> HttpBuilder.get
        |> HttpBuilder.withHeader "Accept" "application/json"
        |> withOptionalSyncTokenHeader syncToken
        |> HttpBuilder.withExpectJson decoder
        |> HttpBuilder.toTask
        |> Task.onError convertError


withOptionalSyncTokenHeader syncToken builder =
    case Maybe.map fromSyncToken syncToken of
        Just val ->
            HttpBuilder.withHeader "X-Sync-Token" val builder

        Nothing ->
            builder
