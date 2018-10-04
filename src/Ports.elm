port module Ports exposing (InMsg(..), receive, startNewSearch)

import Data.Search as Search exposing (Search)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value, null)


startNewSearch : Cmd msg
startNewSearch =
    send StartNewSearch


receiveNewSearchId : (Search.Id -> msg) -> Sub msg
receiveNewSearchId handler =
    receive
        (\result ->
            case result of
                Ok msg ->
                    case msg of
                        NewSearchId id ->
                            handler id

                        _ ->
                            handler "foo"

                Err _ ->
                    handler "foo"
        )



---- INTERNAL -----------------------------------------------------------------


type OutMsg
    = StartNewSearch


send : OutMsg -> Cmd msg
send outMsg =
    let
        ( tag, data ) =
            case outMsg of
                StartNewSearch ->
                    ( "StartNewSearch", null )
    in
    sendMsg { tag = tag, data = data }


type InMsg
    = NewSearchId Search.Id
    | Other


msgDecoder : Decoder InMsg
msgDecoder =
    let
        decoder tag =
            case tag of
                "NewSearchId" ->
                    Decode.field "data" Decode.string
                        |> Decode.map NewSearchId

                _ ->
                    Decode.fail ("Unknown incoming message with tag " ++ tag ++ ".")
    in
    Decode.field "tag" Decode.string
        |> Decode.andThen decoder


receive : (Result Decode.Error InMsg -> msg) -> Sub msg
receive handler =
    let
        decode msg =
            Decode.decodeValue msgDecoder msg
                |> handler
    in
    receiveMsg decode


port sendMsg : { tag : String, data : Value } -> Cmd msg


port receiveMsg : (Value -> msg) -> Sub msg
