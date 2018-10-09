module Data.Name exposing (Name(..), name, namespace)

{-| A namespaced name.
-}


type Name
    = Name String String


namespace : Name -> String
namespace (Name x _) =
    x


name : Name -> String
name (Name _ x) =
    x
