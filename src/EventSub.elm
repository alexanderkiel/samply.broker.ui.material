module EventSub exposing
    ( EventSub
    , Topic
    , Callback
    , new
    , batch
    , none
    , map
    , State
    , init
    , Msg(..)
    , Effect(..)
    , update
    , decoder
    , encodeEffect
    , effectMsg
    )

{-| Event Subscriptions


# Client API

Use this API from modules which like to subscribe to events.

@docs EventSub
@docs Topic
@docs Callback
@docs new
@docs batch
@docs none
@docs map


# State

@docs State
@docs init


# Updates

This module communicates via incoming and outgoing messages. Those messages
typically go though a web socket but don't need to.

@docs Msg
@docs Effect
@docs update
@docs decoder
@docs encodeEffect
@docs effectMsg

-}

import Data.Event as Event exposing (Event)
import Data.Name exposing (Name(..))
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required, resolve)
import Json.Encode as Encode exposing (Value)
import Tuple.Extra as TupleExtra



---- CLIENT API ---------------------------------------------------------------


{-| Event subscription.

    Can be created with `subscribe` or `none` and combined with `batch`.

-}
type EventSub msg
    = SingleSub Topic (Callback msg)
    | BatchSub (List (EventSub msg))
    | NoSub


type alias Topic =
    List String


type alias Callback msg =
    Event -> msg


{-| Creates a new subscription.
-}
new : Topic -> Callback msg -> EventSub msg
new topic callback =
    SingleSub topic callback


{-| Combines a list of subscriptions into a batch subscription.
-}
batch : List (EventSub msg) -> EventSub msg
batch =
    List.foldl batchHelp NoSub


batchHelp : EventSub msg -> EventSub msg -> EventSub msg
batchHelp sub res =
    case sub of
        SingleSub _ _ ->
            case res of
                SingleSub _ _ ->
                    BatchSub [ sub, res ]

                BatchSub subs ->
                    BatchSub (sub :: subs)

                NoSub ->
                    sub

        BatchSub subs ->
            List.foldl batchHelp res subs

        NoSub ->
            res


{-| Creates a not existing or empty subscription.
-}
none : EventSub msg
none =
    NoSub


{-| Lifts the message type of a subscription.
-}
map : (a -> msg) -> EventSub a -> EventSub msg
map tagger sub =
    case sub of
        SingleSub topic callback ->
            SingleSub topic (tagger << callback)

        BatchSub subs ->
            BatchSub <| List.map (map tagger) subs

        NoSub ->
            NoSub



---- STATE --------------------------------------------------------------------


{-| Internal state of this module.
-}
type State msg
    = State (StateRecord msg)


type alias StateRecord msg =
    { subStates : SubStateDict msg }


init : State msg
init =
    State <| { subStates = Dict.empty }


type alias SubStateDict msg =
    Dict Topic (SubState msg)


{-| State of a subscription.
-}
type SubState msg
    = SubscriptionInProgress (Callback msg)
    | Subscribed (Callback msg)
    | UnSubscriptionInProgress (Callback msg)


type Msg msg
    = InMsg InMsg
    | EventSub (EventSub msg)


{-| Incoming messages.
-}
type InMsg
    = InSubscribed Topic
    | InUnSubscribed Topic
    | Event Topic Event


{-| Outgoing messages.
-}
type Effect msg
    = Subscribe Topic
    | UnSubscribe Topic
    | Call msg
    | BatchEffect (List (Effect msg))
    | NoEffect


batchEffect : List (Effect msg) -> Effect msg
batchEffect =
    List.foldl batchEffectHelp NoEffect


batchEffectHelp : Effect msg -> Effect msg -> Effect msg
batchEffectHelp e res =
    case e of
        BatchEffect es ->
            List.foldl batchEffectHelp res es

        NoEffect ->
            res

        _ ->
            case res of
                BatchEffect es ->
                    BatchEffect (e :: es)

                NoEffect ->
                    e

                _ ->
                    BatchEffect [ e, res ]


update : Msg msg -> State msg -> ( State msg, Effect msg )
update msg (State state) =
    Tuple.mapFirst State <|
        case msg of
            InMsg inMsg ->
                onMessage inMsg state

            EventSub eventSub ->
                mergeEventSub eventSub state


{-| Merge subscriptions with the state.

    The resulting state will reflect the given subscriptions tagged with there
    state regarding the server side.

    The resulting effect has to be encoded using `encodeEffect` and send to the
    server.

-}
mergeEventSub : EventSub msg -> StateRecord msg -> ( StateRecord msg, Effect msg )
mergeEventSub sub state =
    let
        -- merge the current subscription states with the given subscriptions
        -- insert/update/remove cases will be handled
        mergeIntoSubStates : SubDict msg -> ( SubStateDict msg, Effect msg )
        mergeIntoSubStates subs =
            subStateMerge state.subStates subs ( Dict.empty, NoEffect )

        subStateMerge =
            Dict.merge
                (TupleExtra.partial2 subStateRemove)
                (TupleExtra.partial3 subStateUpdate)
                (TupleExtra.partial2 subStateInsert)

        -- inserts a new subscription with its callback
        -- spawns a `Subscribe` effect
        subStateInsert topic callback =
            ( Dict.insert topic (SubscriptionInProgress callback)
            , batchEffectHelp (Subscribe topic)
            )

        -- updates an old subscription state
        -- the new state will be still `Subscribed` or a `SubscriptionInProgress`
        -- the callback will be updated in every case
        -- spawns a `Subscribe` effect if unsubscription was already in progress
        subStateUpdate topic oldSubState newCallback =
            case oldSubState of
                SubscriptionInProgress _ ->
                    ( Dict.insert topic (SubscriptionInProgress newCallback)
                    , identity
                    )

                Subscribed _ ->
                    ( Dict.insert topic (Subscribed newCallback)
                    , identity
                    )

                UnSubscriptionInProgress _ ->
                    ( Dict.insert topic (SubscriptionInProgress newCallback)
                    , batchEffectHelp (Subscribe topic)
                    )

        -- keeps the old subscription state because it will only be removed by
        -- unsubscription acknowledgement
        -- spawns an `UnSubscribe` effect unless already spawned
        subStateRemove topic subState =
            case subState of
                SubscriptionInProgress callback ->
                    ( Dict.insert topic (UnSubscriptionInProgress callback)
                    , batchEffectHelp (UnSubscribe topic)
                    )

                Subscribed callback ->
                    ( Dict.insert topic (UnSubscriptionInProgress callback)
                    , batchEffectHelp (UnSubscribe topic)
                    )

                UnSubscriptionInProgress _ ->
                    ( Dict.insert topic subState
                    , identity
                    )
    in
    initSubs sub
        |> mergeIntoSubStates
        |> Tuple.mapFirst (\newSubStates -> { state | subStates = newSubStates })


onMessage : InMsg -> StateRecord msg -> ( StateRecord msg, Effect msg )
onMessage message state =
    case message of
        InSubscribed topic ->
            ( { state | subStates = Dict.update topic (Maybe.map setSubscribed) state.subStates }
            , NoEffect
            )

        InUnSubscribed topic ->
            ( { state | subStates = Dict.remove topic state.subStates }
            , NoEffect
            )

        Event topic event ->
            let
                effect =
                    state.subStates
                        |> Dict.get topic
                        |> Maybe.map getCallback
                        |> Maybe.map (\c -> c event)
                        |> Maybe.map Call
                        |> Maybe.withDefault NoEffect
            in
            ( state, effect )


getCallback : SubState msg -> Callback msg
getCallback subState =
    case subState of
        SubscriptionInProgress callback ->
            callback

        Subscribed callback ->
            callback

        UnSubscriptionInProgress callback ->
            callback


setSubscribed : SubState msg -> SubState msg
setSubscribed subState =
    case subState of
        SubscriptionInProgress callback ->
            Subscribed callback

        Subscribed _ ->
            subState

        UnSubscriptionInProgress callback ->
            Subscribed callback


encodeEffect : Effect msg -> Maybe Value
encodeEffect effect =
    let
        message type_ topic =
            Encode.object
                [ ( "type", Encode.string type_ )
                , ( "topic", Encode.list Encode.string topic )
                ]
    in
    case effect of
        Subscribe topic ->
            Just <| message "subscribe" topic

        UnSubscribe topic ->
            Just <| message "unsubscribe" topic

        Call _ ->
            Nothing

        BatchEffect effects ->
            Just <|
                Encode.object
                    [ ( "type", Encode.string "batch" )
                    , ( "messages", Encode.list identity <| List.filterMap encodeEffect effects )
                    ]

        NoEffect ->
            Nothing


effectMsg : Effect msg -> Maybe msg
effectMsg effect =
    case effect of
        Call msg ->
            Just msg

        _ ->
            Nothing


decoder : Decoder InMsg
decoder =
    let
        topicDecoder =
            Decode.list Decode.string

        toEvent topic data =
            case topic of
                namespace :: name :: _ ->
                    Decode.succeed <|
                        Event topic
                            { name = Name namespace name
                            , data = data
                            }

                _ ->
                    Decode.fail "Invalid topic."

        toMessage type_ =
            case type_ of
                "subscribed" ->
                    Decode.succeed InSubscribed
                        |> required "topic" topicDecoder

                "unsubscribed" ->
                    Decode.succeed InUnSubscribed
                        |> required "topic" topicDecoder

                "event" ->
                    Decode.succeed toEvent
                        |> required "topic" topicDecoder
                        |> required "data" Decode.value
                        |> resolve

                _ ->
                    Decode.fail <| "Unknown message type " ++ type_ ++ "."
    in
    Decode.succeed toMessage
        |> required "type" Decode.string
        |> resolve


type alias SubDict msg =
    Dict Topic (Callback msg)


initSubs : EventSub msg -> SubDict msg
initSubs sub =
    initNewSubsHelp sub Dict.empty


initNewSubsHelp : EventSub msg -> SubDict msg -> SubDict msg
initNewSubsHelp sub dict =
    case sub of
        SingleSub topic callback ->
            Dict.insert topic callback dict

        BatchSub subs ->
            List.foldl initNewSubsHelp dict subs

        NoSub ->
            dict