module Data.Event exposing (Event, decoder)

import Data.Name exposing (Name(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, resolve)
import Json.Encode as Encode exposing (Value)


type alias Event =
    { name : Name
    , data : Value
    }


decoder : Decoder Event
decoder =
    let
        toEvent namespace name data =
            Event (Name namespace name) data
    in
    Decode.succeed toEvent
        |> required "namespace" Decode.string
        |> required "name" Decode.string
        |> required "data" Decode.value
