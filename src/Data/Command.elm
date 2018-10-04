module Data.Command exposing
    ( Command
    , addStringParam
    , build
    , commandBuilder
    , emptyCommand
    , jsonCommand
    )

import Dict exposing (Dict)
import Json.Encode as Encode


type alias Command =
    { namespace : String
    , name : String
    , params : Encode.Value
    }


type alias CommandBuilder =
    { namespace : String
    , name : String
    , params : Dict String String
    }


{-| Creates a new command builder.

    See: `addStringParam` and `build`.

-}
commandBuilder : String -> String -> CommandBuilder
commandBuilder namespace name =
    { namespace = namespace
    , name = name
    , params = Dict.empty
    }


{-| Creates a new command with a JSON value.

    Note that the JSON value has to be an object, which can't be enforced by the
    type system.

-}
jsonCommand : String -> String -> Encode.Value -> Command
jsonCommand namespace name params =
    { namespace = namespace
    , name = name
    , params = params
    }


{-| Creates a new command without any parameters.
-}
emptyCommand : String -> String -> Command
emptyCommand namespace name =
    { namespace = namespace
    , name = name
    , params = Encode.object []
    }


addStringParam : String -> String -> CommandBuilder -> CommandBuilder
addStringParam key val command =
    { command | params = Dict.insert key val command.params }


build : CommandBuilder -> Command
build { namespace, name, params } =
    { namespace = namespace
    , name = name
    , params = Encode.dict identity Encode.string params
    }
