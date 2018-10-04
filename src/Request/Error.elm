module Request.Error exposing
    ( Error(..)
    , FaultId
    , convertError
    , logError
    , notFoundError
    )

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)
import Url as Url exposing (Url)


{-| Error

    Errors are modeled after Cognitect Anomalies were the user of the UI is
    considered the caller and the UI itself together with the backend service
    is the callee. That means that coding errors of the UI are always the UI's
    fault and not the users fault. So for example, we never tell the user, that
    he has wrongly encoded a JSON payload.

    * Forbidden - The current user has no permission to read data or perform a
                  command. Normally this shouldn't happen, because the UI won't
                  allow such a request in the first place, but it can happen if
                  the user loses permission in between.

    * NotFound - The entity requested was not found. Normally this shouldn't
                 happen, because the UI won't allow such a request in the first
                 place, but it can happen if the entity was deleted in between.

    * Fault - The UI or the backend service did something wrong. This problem
              has to be solved by fixing a coding or deployment error os some
              kind. Action by a software developer or ops personal is required.
              The user can't do anything about it. We provide a fault
              identifier the user can use to contact the service runner in order
              to get information whether the problem is resolved. However the
              user doesn't get any detail information about the problem. We save
              that detail information under the fault identifier.

    * Busy - The backend service is busy. The user should retry later.

    * Offline - The users browser is offline.

-}
type Error
    = Forbidden
    | NotFound
    | LoggedFault FaultId
    | Fault
    | Busy
    | Offline


type alias FaultId =
    String


{-| Converts an HTTP error into our own error type.

    There are five different HTTP error types which are handled in the following
    ways:

    * BadUrl - the bad URL is logged to the `/api/error-log` endpoint and the
               resulting LoggedFault is returned.

    * Timeout - a Busy error is returned.

    * NetworkError - an Offline error is returned.

    * BadStatus - on 403, Forbidden is returned. On 404, NotFound is returned.
                  On 503 and 504, Busy is returned. Otherwise, a LoggedFault is
                  tried to decode from the response and is returned. If the
                  response doesn't contain a LoggedFault (because it originated
                  from an intermediate or there is a problem in the backend
                  service), the response body is logged to the `/api/error-log`
                  endpoint and the resulting LoggedFault is returned.

    * BadPayload - the debug message is logged to the `/api/error-log` endpoint
                   and the resulting LoggedFault is returned.

    In case the error logging to the `/api/error-log` endpoint fails itself, a
    Fault is returned.

-}
convertError : Http.Error -> Task Error a
convertError error =
    case error of
        Http.BadUrl url ->
            logError (badUrlError url)
                |> Task.mapError (\_ -> Fault)
                |> Task.andThen Task.fail

        Http.Timeout ->
            Task.fail Busy

        Http.NetworkError ->
            Task.fail Offline

        Http.BadStatus { status, body } ->
            case status.code of
                403 ->
                    Task.fail Forbidden

                404 ->
                    Task.fail NotFound

                503 ->
                    Task.fail Busy

                504 ->
                    Task.fail Busy

                _ ->
                    case Decode.decodeString faultDecoder body of
                        Ok fault ->
                            Task.fail fault

                        Err _ ->
                            logError (nonLoggedError body)
                                |> Task.mapError (\_ -> Fault)
                                |> Task.andThen Task.fail

        Http.BadPayload errorMessage response ->
            logError (badPayloadError errorMessage response)
                |> Task.mapError (\_ -> Fault)
                |> Task.andThen Task.fail


faultDecoder : Decoder Error
faultDecoder =
    Decode.succeed LoggedFault
        |> required "fault-id" Decode.string


{-| Logs the error event to the `/api/error-log` endpoint.

    Returns either a LoggedFault on success or an HTTP error if something did go
    wrong.

-}
logError : Value -> Task Http.Error Error
logError error =
    "/api/error-log"
        |> HttpBuilder.post
        |> HttpBuilder.withJsonBody error
        |> HttpBuilder.withExpectJson faultDecoder
        |> HttpBuilder.toTask


badUrlError : String -> Value
badUrlError url =
    Encode.object
        [ ( "type", Encode.string "bad-url" )
        , ( "url", Encode.string url )
        ]


nonLoggedError : String -> Value
nonLoggedError errorMessage =
    Encode.object
        [ ( "type", Encode.string "non-logged-error" )
        , ( "error-message", Encode.string errorMessage )
        ]


notFoundError : Url -> Value
notFoundError url =
    Encode.object
        [ ( "type", Encode.string "not-found" )
        , ( "url", Encode.string (Url.toString url) )
        ]


badPayloadError : String -> Http.Response String -> Value
badPayloadError errorMessage { url, status, body } =
    Encode.object
        [ ( "type", Encode.string "bad-payload" )
        , ( "error-message", Encode.string errorMessage )
        , ( "url", Encode.string url )
        , ( "status", Encode.int status.code )
        , ( "body", Encode.string body )
        ]
