module Types exposing (..)

import Json.Decode as Decode exposing (Decoder, bool, index, int, list, map2, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http


type Msg
    = GotSchedule (WebData Schedule)
    | PatchedDraws (WebData SavedDraws)
    | DiscardChanges
    | UpdateDrawLabel Int String
    | UpdateDrawStartsAt Int String
    | UpdateDrawAttendance Int String
    | SelectedGame Int DrawSheet String
    | AddDraw
    | DeleteDraw Int
    | Save


type alias Model =
    { flags : Flags
    , schedule : WebData Schedule
    , changed : Bool
    , validated : Bool
    , savedDraws : WebData SavedDraws
    }


type alias Flags =
    { url : String
    }


type alias Schedule =
    { settings : Settings
    , sheets : List String
    , teams : List Team
    , games : List Game
    , draws : List Draw
    }


type alias SavedDraws =
    { draws : List Draw }


type alias Settings =
    { hasAttendance : Bool }


type alias Team =
    { id : Int
    , name : String
    }


type alias Game =
    { id : Int
    , teamIds : ( Int, Int )
    , disabled : Bool
    }


type alias DrawLabel =
    { value : String
    , changed : Bool
    , valid : Bool
    }


type alias DrawStartsAt =
    { value : String
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


scheduleDecoder : Decoder Schedule
scheduleDecoder =
    Decode.succeed Schedule
        |> required "settings" settingsDecoder
        |> required "sheets" (list string)
        |> required "teams" (list teamDecoder)
        |> required "games" (list gameDecoder)
        |> required "draws" (list drawDecoder)


settingsDecoder : Decoder Settings
settingsDecoder =
    Decode.succeed Settings
        |> optional "has_attendance" bool False


teamDecoder : Decoder Team
teamDecoder =
    Decode.succeed Team
        |> required "id" int
        |> required "name" string


gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed Game
        |> required "id" int
        |> required "team_ids" teamIdsDecoder
        |> hardcoded False


teamIdsDecoder : Decoder ( Int, Int )
teamIdsDecoder =
    map2 Tuple.pair (index 0 int) (index 1 int)


savedDrawsDecoder : Decoder SavedDraws
savedDrawsDecoder =
    Decode.succeed SavedDraws
        |> required "draws" (list drawDecoder)


drawDecoder : Decoder Draw
drawDecoder =
    Decode.succeed Draw
        |> required "id" (nullable int)
        |> required "label" (string |> Decode.map (\val -> DrawLabel val False True))
        |> required "starts_at" (string |> Decode.map (\val -> DrawStartsAt val False True))
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


encodeDraws : List Draw -> Encode.Value
encodeDraws draws =
    Encode.object
        [ ( "draws", Encode.list encodeDraw draws ) ]


encodeDraw : Draw -> Encode.Value
encodeDraw draw =
    Encode.object
        [ ( "id", maybe Encode.int draw.id )
        , ( "label", Encode.string draw.label.value )
        , ( "starts_at", Encode.string draw.startsAt.value )
        , ( "attendance", maybe Encode.int draw.attendance.value )
        , ( "draw_sheets", Encode.list encodeDrawSheet draw.drawSheets )
        ]


encodeDrawSheet : DrawSheet -> Encode.Value
encodeDrawSheet drawSheet =
    Encode.object
        [ ( "sheet", Encode.int drawSheet.sheet )
        , ( "game_id", maybe Encode.int drawSheet.gameId )
        ]


encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe encoder maybe =
    case maybe of
        Just value ->
            encoder value

        Nothing ->
            Encode.null
