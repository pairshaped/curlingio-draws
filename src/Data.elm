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
        List.repeat 24 0
            |> List.indexedMap (\n _ -> DropDown (n + 1) Nothing)
    , options =
        [ Option 1 "One" False
        , Option 2 "Two" False
        , Option 3 "Three" False
        , Option 4 "Four" False
        , Option 5 "Five" False
        , Option 6 "Six" False
        , Option 7 "Seven" False
        , Option 8 "Eight" False
        , Option 9 "Nine" False
        , Option 10 "Ten" False
        , Option 11 "Eleven" False
        , Option 12 "Twelve" False
        , Option 13 "Thirteen" False
        , Option 14 "Fourteen" False
        , Option 15 "Fifteen" False
        , Option 16 "Sixteen" False
        , Option 17 "Seventeen" False
        , Option 18 "Eighteen" False
        , Option 19 "Nineteen" False
        , Option 20 "Twenty" False
        , Option 21 "Twenty-one" False
        , Option 22 "Twenty-two" False
        , Option 23 "Twenty-three" False
        , Option 24 "Twenty-four" False
        , Option 25 "Twenty-five" False
        , Option 26 "Twenty-six" False
        , Option 27 "Twenty-seven" False
        , Option 28 "Twenty-eight" False
        , Option 29 "Twenty-nine" False
        , Option 30 "Thirty" False
        , Option 31 "Thirty-one" False
        , Option 32 "Thirty-two" False
        ]
    }
