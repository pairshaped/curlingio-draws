module Helpers exposing (..)

import List.Extra
import RemoteData exposing (RemoteData(..))
import RemoteData.Http
import Types exposing (..)


populateDrawSheetValues : Model -> Model
populateDrawSheetValues model =
    let
        gameNameById id games =
            case List.Extra.find (\game -> game.id == id) games of
                Just game ->
                    gameName game

                Nothing ->
                    ""

        updatedDrawSheet schedule drawSheet =
            { drawSheet
                | value =
                    case drawSheet.gameId of
                        Just gameId ->
                            gameNameById gameId schedule.games

                        Nothing ->
                            ""
            }

        updatedDraw schedule draw =
            { draw | drawSheets = List.map (updatedDrawSheet schedule) draw.drawSheets }

        updatedDraws schedule =
            List.map (updatedDraw schedule) schedule.draws

        updatedSchedule =
            case model.schedule of
                Success decodedSchedule ->
                    Success { decodedSchedule | draws = updatedDraws decodedSchedule }

                _ ->
                    model.schedule
    in
    { model | schedule = updatedSchedule }


updateGames : Model -> Model
updateGames model =
    let
        -- Loop through the games setting each to disabled if a matching drawSheet value is the same as it's name
        gameIsSelectedInDraw game draw =
            case List.Extra.find (\ds -> ds.value == gameName game) draw.drawSheets of
                Just drawSheet ->
                    True

                Nothing ->
                    False

        updatedGame schedule game =
            if List.any (gameIsSelectedInDraw game) schedule.draws then
                { game | disabled = True }

            else
                { game | disabled = False }

        updatedGames schedule =
            List.map (updatedGame schedule) schedule.games

        updatedSchedule =
            case model.schedule of
                Success decodedSchedule ->
                    Success { decodedSchedule | games = updatedGames decodedSchedule }

                _ ->
                    model.schedule
    in
    { model | schedule = updatedSchedule }


getSchedule : String -> Cmd Msg
getSchedule url =
    RemoteData.Http.get url GotSchedule scheduleDecoder


patchDraws : String -> List Draw -> Cmd Msg
patchDraws url draws =
    RemoteData.Http.patch url PatchedDraws savedDrawsDecoder (encodeDraws draws)


drawLabelIsValid : List Draw -> String -> Bool
drawLabelIsValid draws value =
    not
        (value
            == ""
            || List.any (\draw -> draw.label.value == value) draws
        )


drawStartsAtIsValid : List Draw -> String -> Bool
drawStartsAtIsValid draws value =
    not
        (value
            == ""
            || List.any (\draw -> draw.startsAt.value == value) draws
        )


drawAttendanceIsValid : String -> Bool
drawAttendanceIsValid value =
    (value == "")
        || (case String.toInt value of
                Just int ->
                    int < 100000 && int >= 0

                Nothing ->
                    False
           )


gameName : Game -> String
gameName game =
    game.name ++ " - " ++ game.stageName


validateDrawSheets : Schedule -> Draw -> Draw
validateDrawSheets schedule draw =
    let
        findGameByName name =
            List.Extra.find (\game -> gameName game == name) schedule.games

        validateDrawSheet drawSheet =
            case findGameByName drawSheet.value of
                Just game ->
                    { drawSheet | gameId = Just game.id, valid = not (teamsAlreadyAssignedInDraw game draw schedule.games) }

                Nothing ->
                    if drawSheet.value == "" then
                        { drawSheet | gameId = Nothing, valid = True }

                    else
                        { drawSheet | gameId = Nothing, valid = False }

        validatedDrawSheets =
            List.map validateDrawSheet draw.drawSheets
    in
    { draw | drawSheets = validatedDrawSheets }


teamsAlreadyAssignedInDraw : Game -> Draw -> List Game -> Bool
teamsAlreadyAssignedInDraw onGame draw games =
    let
        gameInDraw : Game -> Bool
        gameInDraw game =
            List.any
                (\ds ->
                    case ds.gameId of
                        Just id ->
                            id /= onGame.id && id == game.id

                        Nothing ->
                            False
                )
                draw.drawSheets

        otherDrawGames : List Game
        otherDrawGames =
            List.filter (\g -> gameInDraw g) games

        teamsInDraw : List Int
        teamsInDraw =
            let
                topTeams =
                    List.map (\g -> Tuple.first g.teamIds) otherDrawGames

                bottomTeams =
                    List.map (\g -> Tuple.second g.teamIds) otherDrawGames
            in
            List.append topTeams bottomTeams
    in
    List.member (Tuple.first onGame.teamIds) teamsInDraw
        || List.member (Tuple.second onGame.teamIds) teamsInDraw


validForSave : Model -> Bool
validForSave model =
    let
        drawSheetValid drawSheet =
            drawSheet.valid

        drawValid draw =
            List.all drawSheetValid draw.drawSheets
                && draw.label.valid
                && draw.startsAt.valid
                && draw.attendance.valid

        drawsValid schedule =
            List.all drawValid schedule.draws
    in
    case model.schedule of
        Success decodedSchedule ->
            drawsValid decodedSchedule

        _ ->
            False
