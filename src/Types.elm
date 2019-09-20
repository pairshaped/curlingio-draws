module Types exposing (..)

import Http
import Json.Decode exposing (Decoder, bool, field, index, int, list, map2, map3, map4, map5, maybe, string)


type Msg
    = GotData (Result Http.Error Data)
    | SaveData
    | AddDraw
    | SelectedItem Draw DrawSheet String
    | UpdateDrawLabel Draw String
    | UpdateDrawStartsAt Draw String
    | UpdateDrawAttendance Draw String


type alias Flags =
    { url : String
    }


type RemoteData
    = NotAsked
    | Loading
    | Failure String
    | Success Data


type alias Model =
    { flags : Flags
    , data : RemoteData
    }


type alias Data =
    { sheets : List String
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
    { id : Maybe Int
    , label : Maybe String
    , startsAt : Maybe String
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
        (field "sheets" (list string))
        (field "games" (list gameDecoder))
        (field "draws" (list drawDecoder))


gameDecoder : Decoder Game
gameDecoder =
    map4 Game
        (field "id" int)
        (field "name" string)
        (field "teams" teamsDecoder)
        (field "disabled" bool)


teamsDecoder : Decoder ( Int, Int )
teamsDecoder =
    map2 Tuple.pair (index 0 int) (index 1 int)


drawDecoder : Decoder Draw
drawDecoder =
    map5 Draw
        (maybe (field "id" int))
        (maybe (field "label" string))
        (maybe (field "starts_at" string))
        (maybe (field "attendance" int))
        (field "draw_sheets" (list drawSheetDecoder))


drawSheetDecoder : Decoder DrawSheet
drawSheetDecoder =
    map3 DrawSheet
        (field "sheet" int)
        (maybe (field "game_id" int))
        (field "value" string)
