module Request.Search exposing (search)

import Data.Search exposing (Id, Search, decoder)
import HttpBuilder
import Request.Error exposing (Error, convertError)
import Task exposing (Task)
import Url.Builder as Builder


search : Id -> Task Error Search
search id =
    Builder.absolute [ "api", "searches", id ] []
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decoder
        |> HttpBuilder.toTask
        |> Task.onError convertError
