module Request.Mdr exposing
    ( dataElement
    , dataElementGroupMembers
    , mdrRoot
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


mdrRoot : String
mdrRoot =
    "https://mdr.germanbiobanknode.de/v3/api/mdr"


namespaceMembers : String -> Task Error (List DataElementGroup)
namespaceMembers name =
    Builder.crossOrigin mdrRoot [ "namespaces", name, "members" ] []
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson (resultsDecoder (Decode.list DataElementGroup.decoder))
        |> HttpBuilder.toTask
        |> Task.onError convertError


dataElementGroupMembers : Urn -> Task Error (List DataElement)
dataElementGroupMembers urn =
    Builder.crossOrigin mdrRoot [ "dataelementgroups", urn, "members" ] []
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson (resultsDecoder (Decode.list DataElement.decoder))
        |> HttpBuilder.toTask
        |> Task.onError convertError


dataElement : Urn -> Task Error DataElementDetail
dataElement urn =
    Builder.crossOrigin mdrRoot [ "dataelements", urn ] []
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson DataElement.detailDecoder
        |> HttpBuilder.toTask
        |> Task.onError convertError


resultsDecoder : Decoder a -> Decoder a
resultsDecoder decoder =
    Decode.field "results" decoder
