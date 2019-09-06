module Data exposing (..)


type alias DropDown =
    { id : Int
    , selectedOption : Maybe Option
    }


type alias Option =
    { id : Int
    , name : String
    , disabled : Bool
    }


type alias Model =
    { dropDowns : List DropDown
    , options : List Option
    }


init =
    { dropDowns =
        List.repeat 256 0
            |> List.indexedMap (\n _ -> DropDown (n + 1) Nothing)
    , options =
        List.repeat 256 0
            |> List.indexedMap (\n _ -> Option (n + 1) ("Label for " ++ String.fromInt (n + 1)) False)
    }
