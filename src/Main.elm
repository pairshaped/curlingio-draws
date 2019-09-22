module Main exposing (..)

import Browser
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
    ( Model flags NotAsked, getData flags.url )



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
                    ( { model | data = Failure errorMessage }, Cmd.none )

        RevertAllChanges ->
            ( { model | data = Loading }, getData model.flags.url )

        UpdateDrawLabel onDraw newLabel ->
            let
                updatedDraw draw =
                    if draw.id == onDraw.id then
                        { draw | label = Just newLabel, labelChanged = True }

                    else
                        draw

                updatedDraws draws =
                    List.map updatedDraw draws

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.draws }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData }, Cmd.none )

        UpdateDrawStartsAt onDraw newStartsAt ->
            let
                updatedDraw draw =
                    if draw.id == onDraw.id then
                        { draw | startsAt = Just newStartsAt, startsAtChanged = True }

                    else
                        draw

                updatedDraws draws =
                    List.map updatedDraw draws

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.draws }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData }, Cmd.none )

        UpdateDrawAttendance onDraw newAttendance ->
            let
                updatedDraw draw =
                    if draw.id == onDraw.id then
                        case String.toInt newAttendance of
                            Just attendance ->
                                { draw | attendance = Just attendance, attendanceChanged = True }

                            Nothing ->
                                draw

                    else
                        draw

                updatedDraws draws =
                    List.map updatedDraw draws

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.draws }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData }, Cmd.none )

        SelectedGame onDraw onDrawSheet value ->
            let
                updatedDrawSheet drawSheets drawSheet =
                    if drawSheet.sheet == onDrawSheet.sheet then
                        { drawSheet | gameId = Nothing, value = value, changed = True, problem = False }

                    else
                        drawSheet

                updatedDrawSheets draw =
                    if draw.id == onDraw.id then
                        { draw | drawSheets = List.map (updatedDrawSheet draw.drawSheets) draw.drawSheets }

                    else
                        draw

                updatedDraws draws =
                    List.map updatedDrawSheets draws

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.draws }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData }
                |> updateGames
            , Cmd.none
            )

        AddDraw ->
            let
                newDrawSheet idx sheet =
                    DrawSheet idx Nothing "" True False

                updatedDraws sheets draws =
                    let
                        nextId =
                            Just (List.length draws + 1)

                        nextLabel =
                            Just (String.fromInt (List.length draws + 1))
                    in
                    draws ++ [ Draw nextId nextLabel True Nothing True Nothing True (List.indexedMap newDrawSheet sheets) ]

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.sheets decodedData.draws }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData }, Cmd.none )

        Validate ->
            let
                updatedDrawSheet games draw drawSheet =
                    case List.Extra.find (.name >> (==) drawSheet.value) games of
                        Just game ->
                            if teamsAlreadyAssignedInDraw game draw games then
                                { drawSheet | gameId = Nothing, problem = True }

                            else
                                { drawSheet | gameId = Just game.id, problem = False }

                        Nothing ->
                            { drawSheet | gameId = Nothing, value = "", problem = False }

                updatedDraw games draw =
                    { draw | drawSheets = List.map (updatedDrawSheet games draw) draw.drawSheets }

                updatedDraws games draws =
                    List.map (updatedDraw games) draws

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.games decodedData.draws }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData }, Cmd.none )



-- HELPERS


updateGames : Model -> Model
updateGames model =
    let
        -- Loop through the games setting each to disabled if a matching drawSheet value is the same as it's name
        gameIsSelectedInDraw game draw =
            case List.Extra.find (.value >> (==) game.name) draw.drawSheets of
                Just drawSheet ->
                    True

                Nothing ->
                    False

        gameIsSelected draws game =
            List.map (gameIsSelectedInDraw game) draws
                |> List.member True

        updatedGame draws game =
            { game | disabled = gameIsSelected draws game }

        updatedGames games draws =
            List.map (updatedGame draws) games

        updatedData =
            case model.data of
                Success decodedData ->
                    Success { decodedData | games = updatedGames decodedData.games decodedData.draws }

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
                    List.map (\g -> Tuple.first g.teams) otherDrawGames

                bottomTeams =
                    List.map (\g -> Tuple.second g.teams) otherDrawGames
            in
            List.append topTeams bottomTeams
    in
    List.member (Tuple.first onGame.teams) teamsInDraw
        || List.member (Tuple.second onGame.teams) teamsInDraw



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
