module Helpers exposing (..)

import Http
import Json.Decode as Decode
import List.Extra
import Types exposing (..)


populateDrawSheetValues : Model -> Model
populateDrawSheetValues model =
    let
        updatedDrawSheet data drawSheet =
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
                            data.games
                    of
                        Just game ->
                            nameOfGame data.teams game

                        Nothing ->
                            ""
            }

        updatedDraw data draw =
            { draw | drawSheets = List.map (updatedDrawSheet data) draw.drawSheets }

        updatedDraws data =
            List.map (updatedDraw data) data.draws

        updatedData =
            case model.data of
                Success decodedData ->
                    Success { decodedData | draws = updatedDraws decodedData }

                _ ->
                    model.data
    in
    { model | data = updatedData }


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

        updatedGame data game =
            let
                name =
                    nameOfGame data.teams game
            in
            if gameIsSelected name data.draws game then
                { game | disabled = True }

            else
                { game | disabled = False }

        updatedGames data =
            List.map (updatedGame data) data.games

        updatedData =
            case model.data of
                Success decodedData ->
                    Success { decodedData | games = updatedGames decodedData }

                _ ->
                    model.data
    in
    { model | data = updatedData }


getData : String -> Cmd Msg
getData url =
    Http.get
        { url = url
        , expect = Http.expectJson GotData dataDecoder
        }


saveDraws : String -> List Draw -> Cmd Msg
saveDraws url draws =
    let
        body =
            Http.jsonBody <| encodeDraws draws
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson Saved Decode.string
        , timeout = Nothing
        , tracker = Nothing
        }


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


drawSheetIsValid : Data -> String -> Bool
drawSheetIsValid data value =
    True


validateDrawSheetSelection : List Draw -> Int -> DrawSheet -> List Draw
validateDrawSheetSelection draws drawIndex drawSheet =
    -- TODO make sure the selected value corresponds to a game.
    -- TODO make sure the game hasn't already been assigned.
    -- TODO make sure the game doesn't include a team that's already playing in the draw
    draws


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


findGameByName : Data -> String -> Maybe Game
findGameByName data name =
    data.games
        |> List.filter (\game -> nameOfGame data.teams game == name)
        |> List.head


validateDrawSheets : Data -> Draw -> Draw
validateDrawSheets data draw =
    let
        validateDrawSheet drawSheet =
            case findGameByName data drawSheet.value of
                Just game ->
                    { drawSheet | gameId = Just game.id, valid = not (teamsAlreadyAssignedInDraw game draw data.games) }

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

        drawsValid data =
            List.all drawValid data.draws
    in
    case model.data of
        Success decodedData ->
            drawsValid decodedData

        _ ->
            False
