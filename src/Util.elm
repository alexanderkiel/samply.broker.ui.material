module Util exposing (withCmd)

{-| Utilities

@docs withCmd

-}


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
