module Data.Mdr.DataElementGroup exposing (DataElementGroup, decoder)

import Data.Mdr.Internal.Element as Element exposing (Element)
import Json.Decode exposing (Decoder)


type alias DataElementGroup =
    Element {}


decoder : Decoder DataElementGroup
decoder =
    Element.decoder (\id designation -> { id = id, designation = designation })
