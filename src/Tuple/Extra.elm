module Tuple.Extra exposing (partial2, partial3)


partial2 : (a -> b -> ( c -> e, d -> f )) -> a -> b -> ( c, d ) -> ( e, f )
partial2 f a b =
    let
        ( g, h ) =
            f a b
    in
    Tuple.mapBoth g h


partial3 : (a -> b -> c -> ( d -> f, e -> g )) -> a -> b -> c -> ( d, e ) -> ( f, g )
partial3 f a b c =
    let
        ( g, h ) =
            f a b c
    in
    Tuple.mapBoth g h
