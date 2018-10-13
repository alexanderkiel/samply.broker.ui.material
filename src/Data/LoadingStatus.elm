module Data.LoadingStatus exposing
    ( LoadingStatus(..)
    , isLoaded
    , isNotFound
    , mapLoaded
    , mapLoaded2
    , slowThreshold
    )

import Process
import Request.Error exposing (Error(..))
import Task exposing (Task)


{-| Loading status of some type a

    First it's `Loading` than it's may be `LoadingSlowly` before it results
    either in `Loaded` or `Failed` with `Error`.

    The view can show some load indicator like a spinner on `LoadingSlowly`.

    Should be used for all data loaded over network in page models.

-}
type LoadingStatus a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed Error


isLoaded : LoadingStatus a -> Bool
isLoaded status =
    case status of
        Loading ->
            False

        LoadingSlowly ->
            False

        Loaded search ->
            True

        Failed error ->
            False


isNotFound : LoadingStatus a -> Bool
isNotFound status =
    case status of
        Loading ->
            False

        LoadingSlowly ->
            False

        Loaded search ->
            False

        Failed error ->
            case error of
                NotFound ->
                    True

                _ ->
                    False


mapLoaded : (a -> b) -> LoadingStatus a -> LoadingStatus b
mapLoaded f status =
    case status of
        Loading ->
            Loading

        LoadingSlowly ->
            LoadingSlowly

        Loaded x ->
            Loaded (f x)

        Failed error ->
            Failed error


mapLoaded2 : (a -> ( b, Cmd msg )) -> LoadingStatus a -> ( LoadingStatus b, Cmd msg )
mapLoaded2 f status =
    case status of
        Loading ->
            ( Loading, Cmd.none )

        LoadingSlowly ->
            ( LoadingSlowly, Cmd.none )

        Loaded x ->
            f x |> Tuple.mapFirst Loaded

        Failed error ->
            ( Failed error, Cmd.none )


slowThreshold : Task x ()
slowThreshold =
    Process.sleep 500
