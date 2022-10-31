module DrawSchedule exposing (..)

import Browser
import Helpers exposing (..)
import List.Extra
import RemoteData exposing (RemoteData(..))
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
    ( Model flags NotAsked False True NotAsked Nothing, getSchedule flags.url )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSchedule schedule ->
            ( { model | schedule = schedule }
                |> populateDrawSheetValues
                |> updateGames
            , Cmd.none
            )

        PatchedDraws draws ->
            ( { model | schedule = Loading, savedDraws = draws, changed = False }
            , getSchedule model.flags.url
            )

        DiscardChanges ->
            ( { model | schedule = Loading, changed = False, validated = True }, getSchedule model.flags.url )

        UpdateDrawLabel index newLabel ->
            let
                updatedDrawLabel draws label =
                    { label | value = newLabel, changed = True, valid = drawLabelIsValid draws newLabel }

                updatedDraw draws draw =
                    { draw | label = updatedDrawLabel draws draw.label }

                updatedDraws draws =
                    List.Extra.updateAt index (\draw -> updatedDraw draws draw) draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule.draws }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, changed = True, validated = False }, Cmd.none )

        UpdateDrawStartsAt index newStartsAt ->
            let
                updatedDrawStartsAt draws startsAt =
                    { startsAt | value = newStartsAt, changed = True, valid = drawStartsAtIsValid draws newStartsAt }

                updatedDraw draws draw =
                    { draw | startsAt = updatedDrawStartsAt draws draw.startsAt }

                updatedDraws draws =
                    List.Extra.updateAt index (\draw -> updatedDraw draws draw) draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule.draws }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, changed = True, validated = False }, Cmd.none )

        UpdateDrawAttendance index newAttendance ->
            let
                updatedDrawAttendance attendance =
                    { attendance | value = String.toInt newAttendance, changed = True, valid = drawAttendanceIsValid newAttendance }

                updatedDraw draw =
                    { draw | attendance = updatedDrawAttendance draw.attendance }

                updatedDraws draws =
                    List.Extra.updateAt index (\draw -> updatedDraw draw) draws

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

        DeselectGame index onDrawSheet ->
            let
                updatedDrawSheet draw drawSheet =
                    if drawSheet.sheet == onDrawSheet.sheet then
                        { drawSheet | value = "", gameId = Nothing, changed = False }

                    else
                        drawSheet

                updatedDraw schedule draw =
                    { draw | drawSheets = List.map (updatedDrawSheet draw) draw.drawSheets }
                        |> validateDrawSheets schedule

                updatedDraws schedule =
                    List.Extra.updateAt index (\draw -> updatedDraw schedule draw) schedule.draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, deselectedGame = Just onDrawSheet.value, changed = False }
            , Cmd.none
            )

        ReselectGame index onDrawSheet ->
            let
                updatedDrawSheet draw drawSheet =
                    if drawSheet.sheet == onDrawSheet.sheet && model.changed == False then
                        { drawSheet | value = Maybe.withDefault "" model.deselectedGame, gameId = Nothing, changed = False }

                    else
                        drawSheet

                updatedDraw schedule draw =
                    { draw | drawSheets = List.map (updatedDrawSheet draw) draw.drawSheets }
                        |> validateDrawSheets schedule

                updatedDraws schedule =
                    List.Extra.updateAt index (\draw -> updatedDraw schedule draw) schedule.draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, deselectedGame = Nothing, changed = False, validated = True }
            , Cmd.none
            )

        SelectedGame index onDrawSheet value ->
            let
                updatedDrawSheet draw drawSheet =
                    if drawSheet.sheet == onDrawSheet.sheet then
                        { drawSheet | value = String.trim value, gameId = Nothing, changed = True }

                    else
                        drawSheet

                updatedDraw schedule draw =
                    { draw | drawSheets = List.map (updatedDrawSheet draw) draw.drawSheets }
                        |> validateDrawSheets schedule

                updatedDraws schedule =
                    List.Extra.updateAt index (\draw -> updatedDraw schedule draw) schedule.draws

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
                newDrawSheet index sheet =
                    DrawSheet (index + 1) Nothing "" True True

                updatedDraws sheets draws =
                    let
                        nextLabel =
                            DrawLabel (String.fromInt (List.length draws + 1)) True True

                        nextStartsAt =
                            let
                                previousStartsAt =
                                    case List.Extra.last draws of
                                        Just draw ->
                                            draw.startsAt.value

                                        Nothing ->
                                            ""
                            in
                            DrawStartsAt previousStartsAt True False

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
                sendPatch =
                    case model.schedule of
                        Success decodedSchedule ->
                            patchDraws model.flags.url decodedSchedule.draws

                        _ ->
                            Cmd.none
            in
            ( { model | savedDraws = Loading }, sendPatch )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
