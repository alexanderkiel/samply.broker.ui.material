module Data.Mdr.DataElement exposing
    ( DataElement
    , DataElementDetail
    , Validation(..)
    , Value
    , decoder
    , detailDecoder
    , getUnit
    )

import Data.Mdr.Internal.Element as Element exposing (Element)
import Data.Urn as Urn exposing (Urn)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required, requiredAt, resolve)


type alias DataElement =
    Element {}


decoder : Decoder DataElement
decoder =
    Element.decoder (\id designation -> { id = id, designation = designation })


type alias DataElementDetail =
    Element { validation : Validation }


type Validation
    = Enumerated (List Value)
    | Date
    | Float FloatValidation


type alias FloatValidation =
    { unit : Maybe String }


type alias Value =
    { value : String
    , designation : String
    }


getUnit : DataElementDetail -> Maybe String
getUnit elementDetail =
    case elementDetail.validation of
        Float { unit } ->
            unit

        _ ->
            Nothing


detailDecoder : Decoder DataElementDetail
detailDecoder =
    let
        toDataElementDetail id designation dataType =
            validationDecoder dataType
                |> Decode.map
                    (\validation ->
                        { id = id
                        , designation = designation
                        , validation = validation
                        }
                    )
    in
    Decode.succeed toDataElementDetail
        |> requiredAt [ "identification", "urn" ] Urn.decoder
        |> required "designations" Element.firstDesignationDecoder
        |> requiredAt [ "validation", "datatype" ] Decode.string
        |> resolve


validationDecoder : String -> Decoder Validation
validationDecoder dataType =
    case dataType of
        "enumerated" ->
            Decode.succeed Enumerated
                |> requiredAt [ "validation", "permissible_values" ]
                    (Decode.list valueDecoder)

        "DATE" ->
            Decode.succeed Date

        "FLOAT" ->
            Decode.succeed (FloatValidation >> Float)
                |> requiredAt [ "validation", "unit_of_measure" ]
                    (Decode.maybe nonEmptyStringDecoder)

        _ ->
            Decode.fail <| "Unknown data type: " ++ dataType


valueDecoder : Decoder Value
valueDecoder =
    Decode.succeed Value
        |> required "value" Decode.string
        |> requiredAt [ "meanings" ] Element.firstDesignationDecoder


nonEmptyStringDecoder : Decoder String
nonEmptyStringDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                if String.isEmpty s then
                    Decode.fail "Expecting non-empty string."

                else
                    Decode.succeed s
            )
