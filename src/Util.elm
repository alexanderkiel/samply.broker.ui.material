module Util exposing
    ( withCmd
    , dictUpdateWithCmd
    )

{-| Utilities

@docs withCmd
@docs dictUpdateWithCmd

-}

import Dict exposing (Dict)


{-| Takes a function like the partial application of a typical update function
with the message already applied and returns a function which also carries over
a command from a previous update function result.

    Example:

        update1 : Msg -> Model -> ( Model, Cmd Msg )
        update2 : Msg -> Model -> ( Model, Cmd Msg )

        (update1 msg1)
            |> withCmd (update2 msg2)

-}
withCmd : (m -> ( m, Cmd msg )) -> (( m, Cmd msg ) -> ( m, Cmd msg ))
withCmd f =
    \( m, c1 ) -> f m |> Tuple.mapSecond (\c2 -> Cmd.batch [ c1, c2 ])


dictUpdateWithCmd : comparable -> (Maybe v -> Maybe ( v, Cmd msg )) -> Dict comparable v -> ( Dict comparable v, Cmd msg )
dictUpdateWithCmd targetKey alter dictionary =
    case alter (Dict.get targetKey dictionary) of
        Just ( value, cmd ) ->
            ( Dict.insert targetKey value dictionary, cmd )

        Nothing ->
            ( Dict.remove targetKey dictionary, Cmd.none )
