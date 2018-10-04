module Data.Search exposing (Id, Search, decoder)

import Data.Search.Criterion as Criterion exposing (Criterion)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


type alias Search =
    { id : Id
    , title : String
    , criteria : List Criterion
    }


type alias Id =
    String


decoder : Decoder Search
decoder =
    Decode.succeed Search
        |> required "id" Decode.string
        |> required "title" Decode.string
        |> optional "criteria" (Decode.list Criterion.decoder) []
