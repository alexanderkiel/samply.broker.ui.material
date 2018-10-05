module Request.Mdr exposing
    ( dataElement
    , dataElementGroupMembers
    , namespaceMembers
    )

import Data.Mdr.DataElement as DataElement exposing (DataElement, DataElementDetail)
import Data.Mdr.DataElementGroup as DataElementGroup exposing (DataElementGroup)
import Data.Urn exposing (Urn)
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Request.Error exposing (Error, convertError)
import Task exposing (Task)
import Url.Builder as Builder


namespaceMembers : String -> String -> Task Error (List DataElementGroup)
namespaceMembers mdrRoot name =
    Builder.crossOrigin mdrRoot [ "namespaces", name, "members" ] []
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson (resultsDecoder (Decode.list DataElementGroup.decoder))
        |> HttpBuilder.toTask
        |> Task.onError convertError


dataElementGroupMembers : String -> Urn -> Task Error (List DataElement)
dataElementGroupMembers mdrRoot urn =
    Builder.crossOrigin mdrRoot [ "dataelementgroups", urn, "members" ] []
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson (resultsDecoder (Decode.list DataElement.decoder))
        |> HttpBuilder.toTask
        |> Task.onError convertError


dataElement : String -> Urn -> Task Error DataElementDetail
dataElement mdrRoot urn =
    Builder.crossOrigin mdrRoot [ "dataelements", urn ] []
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson DataElement.detailDecoder
        |> HttpBuilder.toTask
        |> Task.onError convertError


resultsDecoder : Decoder a -> Decoder a
resultsDecoder decoder =
    Decode.field "results" decoder
