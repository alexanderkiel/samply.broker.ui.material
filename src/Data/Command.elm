module Data.Command exposing
    ( Command
    , SyncToken
    , Result
    , CreateResult
    , emptyCommand
    , jsonCommand
    , commandBuilder
    , addStringParam
    , build
    , syncTokenDecoder
    , fromSyncToken
    )

{-| A command is something a subject wants to do in a system.


# Types

@docs Command
@docs SyncToken
@docs Result
@docs CreateResult


# Constructors

@docs emptyCommand
@docs jsonCommand


# CommandBuilder

@docs CommandBuilder
@docs commandBuilder
@docs addStringParam
@docs build


# Other

@docs syncTokenDecoder
@docs fromSyncToken

-}

import Data.Name exposing (Name)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A command consists of a name and params.

    Names have a namespace and so are of a special type.

-}
type alias Command =
    { name : Name
    , params : Encode.Value
    }


{-| A token to sync the reads with the effects of a command.
-}
type SyncToken
    = SyncToken Int


syncTokenDecoder : Decoder SyncToken
syncTokenDecoder =
    Decode.map SyncToken Decode.int


fromSyncToken : SyncToken -> String
fromSyncToken (SyncToken t) =
    String.fromInt t


{-| The result of a command which does an arbitrary side effect.

    See `CreateResult` for the result of a command which created an
    entity.

    The `t` value can be used to fetch at least a state which includes the
    effects of this command.

-}
type alias Result =
    { t : SyncToken }


{-| The result of a command which created an entity.

    The `t` value can be used to fetch at least a state which includes the
    effects of this command.

    The `id` is the identifier of the entity created.

-}
type alias CreateResult =
    { t : SyncToken
    , id : String
    }


type alias CommandBuilder =
    { name : Name
    , params : Dict String String
    }


{-| Creates a new command builder.

    See: `addStringParam` and `build`.

-}
commandBuilder : Name -> CommandBuilder
commandBuilder name =
    { name = name
    , params = Dict.empty
    }


{-| Creates a new command with a JSON value.

    Note that the JSON value has to be an object, which can't be enforced by the
    type system.

-}
jsonCommand : Name -> Encode.Value -> Command
jsonCommand name params =
    { name = name
    , params = params
    }


{-| Creates a new command without any parameters.
-}
emptyCommand : Name -> Command
emptyCommand name =
    { name = name
    , params = Encode.object []
    }


addStringParam : String -> String -> CommandBuilder -> CommandBuilder
addStringParam key val command =
    { command | params = Dict.insert key val command.params }


build : CommandBuilder -> Command
build { name, params } =
    { name = name
    , params = Encode.dict identity Encode.string params
    }
