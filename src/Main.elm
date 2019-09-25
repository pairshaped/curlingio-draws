module Main exposing (..)

import Browser
import Helpers exposing (..)
import Http
import List.Extra
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
    ( Model flags NotAsked False True, getData flags.url )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok data ->
                    ( { model | data = Success data }
                        |> updateGames
                    , Cmd.none
                    )

                Err err ->
                    let
                        errorMessage =
                            case err of
                                Http.BadUrl string ->
                                    "Bad URL used to fetch data: " ++ string

                                Http.Timeout ->
                                    "Network timeout when trying to fetch data."

                                Http.NetworkError ->
                                    "Network error when trying to fetch data."

                                Http.BadStatus int ->
                                    "Bad status response from server when trying to fetch data."

                                Http.BadBody string ->
                                    "Bad body response from server when trying to fetch data: " ++ string
                    in
                    ( { model | data = Failure errorMessage, changed = False, validated = True }, Cmd.none )

        DiscardChanges ->
            ( { model | data = Loading, changed = False, validated = True }, getData model.flags.url )

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

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.draws }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData, changed = True, validated = False }, Cmd.none )

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

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.draws }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData, changed = True, validated = False }, Cmd.none )

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

                updatedData =
                    case model.data of
                        Success decodedData ->
                            if decodedData.hasAttendance then
                                Success { decodedData | draws = updatedDraws decodedData.draws }

                            else
                                model.data

                        _ ->
                            model.data
            in
            ( { model | data = updatedData, changed = True, validated = False }, Cmd.none )

        SelectedGame onDrawIndex onDrawSheet value ->
            let
                updatedDrawSheet drawSheets drawSheet =
                    if drawSheet.sheet == onDrawSheet.sheet then
                        { drawSheet | gameId = Nothing, value = value, changed = True, valid = True }

                    else
                        drawSheet

                updatedDrawSheets index draw =
                    if index == onDrawIndex then
                        { draw | drawSheets = List.map (updatedDrawSheet draw.drawSheets) draw.drawSheets }

                    else
                        draw

                updatedDraws draws =
                    List.indexedMap updatedDrawSheets draws

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.draws }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData, changed = True, validated = False }
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

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.sheets decodedData.draws }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData, changed = True, validated = False }, Cmd.none )

        Validate ->
            let
                updatedLabel draws label =
                    { label
                        | valid =
                            if label.value == "" then
                                False

                            else
                                True
                    }

                updatedStartsAt draws startsAt =
                    { startsAt
                        | valid =
                            if startsAt.value == "" then
                                False

                            else
                                True
                    }

                updatedDrawSheet data draw drawSheet =
                    case List.head (List.filter (\game -> nameOfGame data.teams game == drawSheet.value) data.games) of
                        Just game ->
                            if teamsAlreadyAssignedInDraw game draw data.games then
                                { drawSheet | gameId = Nothing, valid = False }

                            else
                                { drawSheet | gameId = Just game.id, valid = True }

                        Nothing ->
                            { drawSheet | gameId = Nothing, value = "", valid = True }

                updatedDraw data draw =
                    { draw
                        | drawSheets = List.map (updatedDrawSheet data draw) draw.drawSheets
                        , label = updatedLabel data.draws draw.label
                        , startsAt = updatedStartsAt data.draws draw.startsAt
                    }

                updatedDraws data =
                    List.map (updatedDraw data) data.draws

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData }
                |> updateValidated
            , Cmd.none
            )

        Save ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
