module Helpers exposing (..)

import List.Extra
import RemoteData exposing (RemoteData(..))
import RemoteData.Http
import Types exposing (..)


populateDrawSheetValues : Model -> Model
populateDrawSheetValues model =
    let
        updatedDrawSheet schedule drawSheet =
            { drawSheet
                | value =
                    case
                        List.Extra.find
                            (\g ->
                                g.id
                                    == (case drawSheet.gameId of
                                            Just id ->
                                                id

                                            Nothing ->
                                                -1
                                       )
                            )
                            schedule.games
                    of
                        Just game ->
                            nameOfGame schedule.teams game

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
        gameIsSelectedInDraw name game draw =
            case List.Extra.find (\ds -> ds.value == name) draw.drawSheets of
                Just drawSheet ->
                    True

                Nothing ->
                    False

        gameIsSelected name draws game =
            List.map (gameIsSelectedInDraw name game) draws
                |> List.member True

        updatedGame schedule game =
            let
                name =
                    nameOfGame schedule.teams game
            in
            if gameIsSelected name schedule.draws game then
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


nameOfGame : List Team -> Game -> String
nameOfGame teams game =
    let
        teamNameForId id =
            case List.head (List.filter (\team -> team.id == id) teams) of
                Just team ->
                    team.name

                Nothing ->
                    "TBD"
    in
    teamNameForId (Tuple.first game.teamIds)
        ++ " vs "
        ++ teamNameForId (Tuple.second game.teamIds)
        ++ " (G"
        ++ String.fromInt game.id
        ++ ")"


findGameByName : Schedule -> String -> Maybe Game
findGameByName schedule name =
    schedule.games
        |> List.filter (\game -> nameOfGame schedule.teams game == name)
        |> List.head


validateDrawSheets : Schedule -> Draw -> Draw
validateDrawSheets schedule draw =
    let
        validateDrawSheet drawSheet =
            case findGameByName schedule drawSheet.value of
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
