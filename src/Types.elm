module Types exposing (..)

import Http
import Json.Decode exposing (Decoder, field, int, list, maybe, string, succeed)


type alias Flags =
    { url : String
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


type alias Game =
    { id : Int
    , name : String
    , teams : ( Int, Int )
    , disabled : Bool
    }


type alias Data =
    { numberOfSheets : Int
    , games : List Game
    , draws : List Draw
    }


type RemoteData
    = Failure String
    | Loading
    | Success Data


type alias Model =
    { remoteData : RemoteData }


type Msg
    = GotData (Result Http.Error RemoteData)
    | SaveData
    | SelectedItem Draw DrawSheet String
    | UpdateDrawLabel Draw String
    | UpdateAttendance Draw String


dataDecoder : Decoder Data
dataDecoder =
    Json.Decode.map3 Data
        (field "number_of_sheets" int)
        (list "games" gameDecoder)
        (list "draws" drawDecoder)


gameDecoder : Decoder Game
gameDecoder =
    Json.Decode.map4 Game
        (field "id" int)
        (field "name" string)
        (list "teams" int)
        (field "disabled" bool)


drawDecoder : Decoder Draw
drawDecoder =
    Json.Decode.map4 Draw
        (field "id" int)
        (field "starts_at" string)
        (field "attendance" int)
        (list "draw_sheets" drawSheetDecoder)


drawSheetDecoder : Decoder DrawSheet
drawSheetDecoder =
    Json.Decode.map3 DrawSheet
        (field "sheet" int)
        (field "game_id" int)
        (field "value" string)
