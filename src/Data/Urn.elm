module Data.Urn exposing (Urn, decoder)

import Json.Decode as Decode exposing (Decoder)


{-| <https://tools.ietf.org/html/rfc8141>
-}
type alias Urn =
    String


decoder : Decoder Urn
decoder =
    Decode.string
