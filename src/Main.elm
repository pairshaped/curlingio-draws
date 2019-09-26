module Main exposing (..)

import Array
import Browser
import Helpers exposing (..)
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Types exposing (..)
import Views exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags NotAsked False True NotAttempted, getSchedule flags.url )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSchedule result ->
            case result of
                Ok schedule ->
                    ( { model | schedule = Success schedule }
                        |> populateDrawSheetValues
                        |> updateGames
                    , Cmd.none
                    )

                Err err ->
                    let
                        errorMessage =
                            case err of
                                RemoteData.Http.BadUrl string ->
                                    "Bad URL used to fetch schedule: " ++ string

                                RemoteData.Http.Timeout ->
                                    "Network timeout when trying to fetch schedule."

                                RemoteData.Http.NetworkError ->
                                    "Network error when trying to fetch schedule."

                                RemoteData.Http.BadStatus int ->
                                    "Bad status response from server when trying to fetch schedule."

                                RemoteData.Http.BadBody string ->
                                    "Bad body response from server when trying to fetch schedule: " ++ string
                    in
                    ( { model | schedule = Failure errorMessage, changed = False, validated = True }, Cmd.none )

        PatchedDraws result ->
            case result of
                Ok savedDraws ->
                    ( { model | savedDraws = SaveSuccess savedDraws, changed = False }
                    , Cmd.none
                    )

                Err err ->
                    let
                        errorMessage =
                            case err of
                                RemoteData.Http.BadUrl string ->
                                    "Bad URL used to save draws: " ++ string

                                RemoteData.Http.Timeout ->
                                    "Network timeout when trying to save draws."

                                RemoteData.Http.NetworkError ->
                                    "Network error when trying to save draws."

                                RemoteData.Http.BadStatus int ->
                                    "Bad status response from server when trying to save draws."

                                RemoteData.Http.BadBody string ->
                                    "Bad body response from server when trying to save draws: " ++ string
                    in
                    ( { model | savedDraws = SaveFailure errorMessage }, Cmd.none )

        DiscardChanges ->
            ( { model | schedule = Loading, changed = False, validated = True }, getSchedule model.flags.url )

        UpdateDrawLabel onIndex newLabel ->
            let
                updatedDrawLabel draws label =
                    { label | value = newLabel, changed = True, valid = drawLabelIsValid draws newLabel }

                updatedDraw draws index draw =
                    if index == onIndex then
                        { draw | label = updatedDrawLabel draws draw.label }

                    else
                        draw

                updatedDraws draws =
                    List.indexedMap (updatedDraw draws) draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule.draws }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, changed = True, validated = False }, Cmd.none )

        UpdateDrawStartsAt onIndex newStartsAt ->
            let
                updatedDrawStartsAt draws startsAt =
                    { startsAt | value = newStartsAt, changed = True, valid = drawStartsAtIsValid draws newStartsAt }

                updatedDraw draws index draw =
                    if index == onIndex then
                        { draw | startsAt = updatedDrawStartsAt draws draw.startsAt }

                    else
                        draw

                updatedDraws draws =
                    List.indexedMap (updatedDraw draws) draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule.draws }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, changed = True, validated = False }, Cmd.none )

        UpdateDrawAttendance onIndex newAttendance ->
            let
                updatedDrawAttendance attendance =
                    { attendance | value = String.toInt newAttendance, changed = True, valid = drawAttendanceIsValid newAttendance }

                updatedDraw index draw =
                    if index == onIndex then
                        { draw | attendance = updatedDrawAttendance draw.attendance }

                    else
                        draw

                updatedDraws draws =
                    List.indexedMap updatedDraw draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            if decodedSchedule.settings.hasAttendance then
                                Success { decodedSchedule | draws = updatedDraws decodedSchedule.draws }

                            else
                                model.schedule

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, changed = True, validated = False }, Cmd.none )

        SelectedGame onDrawIndex onDrawSheet value ->
            let
                updatedDrawSheet draw drawSheet =
                    if drawSheet.sheet == onDrawSheet.sheet then
                        { drawSheet | value = value, gameId = Nothing, changed = True }

                    else
                        drawSheet

                updatedDrawSheets schedule index draw =
                    if index == onDrawIndex then
                        { draw | drawSheets = List.map (updatedDrawSheet draw) draw.drawSheets }
                            |> validateDrawSheets schedule

                    else
                        draw

                updatedDraws schedule =
                    List.indexedMap (updatedDrawSheets schedule) schedule.draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, changed = True, validated = False }
                |> updateGames
            , Cmd.none
            )

        AddDraw ->
            let
                newDrawSheet idx sheet =
                    DrawSheet idx Nothing "" True True

                updatedDraws sheets draws =
                    let
                        nextLabel =
                            DrawLabel (String.fromInt (List.length draws + 1)) True True

                        nextStartsAt =
                            DrawStartsAt "" True False

                        nextAttendance =
                            DrawAttendance Nothing True True
                    in
                    draws ++ [ Draw Nothing nextLabel nextStartsAt nextAttendance (List.indexedMap newDrawSheet sheets) ]

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule.sheets decodedSchedule.draws }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, changed = True, validated = False }, Cmd.none )

        DeleteDraw index ->
            let
                updatedDraws draws =
                    List.Extra.removeAt index draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule.draws }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, changed = True }, Cmd.none )

        Save ->
            let
                postDraws =
                    case model.schedule of
                        Success decodedSchedule ->
                            saveDraws model.flags.url decodedSchedule.draws

                        _ ->
                            Cmd.none
            in
            ( model, postDraws )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
