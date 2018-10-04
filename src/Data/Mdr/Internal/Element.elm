module Data.Mdr.Internal.Element exposing
    ( Element
    , decoder
    , firstDesignationDecoder
    )

import Data.Urn as Urn exposing (Urn)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Element a =
    { a
        | id : Urn
        , designation : String
    }


decoder : (Urn -> String -> Element a) -> Decoder (Element a)
decoder ctor =
    Decode.succeed ctor
        |> required "id" Urn.decoder
        |> required "designations" firstDesignationDecoder


firstDesignationDecoder : Decoder String
firstDesignationDecoder =
    Decode.list designationDecoder
        |> Decode.andThen firstDesignation


type alias Designation =
    { designation : String }


designationDecoder : Decoder Designation
designationDecoder =
    Decode.succeed Designation
        |> required "designation" Decode.string


firstDesignation : List Designation -> Decoder String
firstDesignation designations =
    case designations of
        head :: _ ->
            Decode.succeed head.designation

        _ ->
            Decode.fail "Expected at least one designation."
