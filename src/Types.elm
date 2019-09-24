module Types exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, bool, index, int, list, map2, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type Msg
    = GotData (Result Http.Error Data)
    | RevertAllChanges
    | UpdateDrawLabel Int String
    | UpdateDrawStartsAt Int String
    | UpdateDrawAttendance Int String
    | SelectedGame Int DrawSheet String
    | AddDraw
    | Validate
    | Save


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
    , changed : Bool
    , validated : Bool
    }


type alias Data =
    { hasAttendance : Bool
    , sheets : List String
    , games : List Game
    , draws : List Draw
    }


type alias Game =
    { id : Int
    , name : String
    , teams : ( Int, Int )
    , disabled : Bool
    }


type alias DrawLabel =
    { value : Maybe String
    , changed : Bool
    , valid : Bool
    }


type alias DrawStartsAt =
    { value : Maybe String
    , changed : Bool
    , valid : Bool
    }


type alias DrawAttendance =
    { value : Maybe Int
    , changed : Bool
    , valid : Bool
    }


type alias Draw =
    { id : Maybe Int
    , label : DrawLabel
    , startsAt : DrawStartsAt
    , attendance : DrawAttendance
    , drawSheets : List DrawSheet
    }


type alias DrawSheet =
    { sheet : Int
    , gameId : Maybe Int
    , value : String
    , changed : Bool
    , valid : Bool
    }


dataDecoder : Decoder Data
dataDecoder =
    Decode.succeed Data
        |> optional "has_attendance" bool False
        |> required "sheets" (list string)
        |> required "games" (list gameDecoder)
        |> required "draws" (list drawDecoder)


gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed Game
        |> required "id" int
        |> required "name" string
        |> required "teams" teamsDecoder
        |> hardcoded False


teamsDecoder : Decoder ( Int, Int )
teamsDecoder =
    map2 Tuple.pair (index 0 int) (index 1 int)


drawDecoder : Decoder Draw
drawDecoder =
    Decode.succeed Draw
        |> required "id" (nullable int)
        |> required "label" (nullable string |> Decode.map (\val -> DrawLabel val False True))
        |> required "starts_at" (nullable string |> Decode.map (\val -> DrawStartsAt val False True))
        |> required "attendance" (nullable int |> Decode.map (\val -> DrawAttendance val False True))
        |> required "draw_sheets" (list drawSheetDecoder)


drawSheetDecoder : Decoder DrawSheet
drawSheetDecoder =
    Decode.succeed DrawSheet
        |> required "sheet" int
        |> required "game_id" (nullable int)
        |> optional "value" string ""
        |> hardcoded False
        |> hardcoded True
