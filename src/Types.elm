module Types exposing (..)

import Http
import Json.Decode exposing (Decoder, bool, field, index, int, list, map2, map3, map4, map5, maybe, string)


type Msg
    = GotData (Result Http.Error RemoteData)
    | SaveData
    | SelectedItem Draw DrawSheet String
    | UpdateDrawLabel Draw String
    | UpdateAttendance Draw String


type alias Flags =
    { url : String
    }


type alias Model =
    { remoteData : RemoteData
    }


type RemoteData
    = Failure String
    | Loading
    | Success Data


type alias Data =
    { numberOfSheets : Int
    , games : List Game
    , draws : List Draw
    }


type alias Game =
    { id : Int
    , name : String
    , teams : ( Int, Int )
    , disabled : Bool
    }


type alias Draw =
    { id : Int
    , label : Maybe String
    , starts_at : Maybe String
    , attendance : Maybe Int
    , drawSheets : List DrawSheet
    }


type alias DrawSheet =
    { sheet : Int
    , gameId : Maybe Int
    , value : String
    }


dataDecoder : Decoder Data
dataDecoder =
    map3 Data
        (field "number_of_sheets" int)
        (list gameDecoder)
        (list drawDecoder)


gameDecoder : Decoder Game
gameDecoder =
    map4 Game
        (field "id" int)
        (field "name" string)
        (field "teams" teamsDecoder)
        (field "disabled" bool)


drawDecoder : Decoder Draw
drawDecoder =
    map5 Draw
        (field "id" int)
        (maybe (field "label" string))
        (maybe (field "starts_at" string))
        (maybe (field "attendance" int))
        (list drawSheetDecoder)


drawSheetDecoder : Decoder DrawSheet
drawSheetDecoder =
    map3 DrawSheet
        (field "sheet" int)
        (maybe (field "game_id" int))
        (field "value" string)


teamsDecoder : Decoder ( Int, Int )
teamsDecoder =
    map2 Tuple.pair (index 0 int) (index 1 int)
