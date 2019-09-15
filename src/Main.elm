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
    ( { flags = flags, remoteData = Loading }, getData flags.url )



-- UPDATE


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

        remoteData =
            case model.remoteData of
                Success decodedData ->
                    Success { decodedData | games = updatedGames decodedData.games decodedData.draws }

                _ ->
                    model.remoteData
    in
    { model | remoteData = remoteData }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedItem onDraw onDrawSheet value ->
            let
                updatedDrawSheet drawSheet =
                    if drawSheet.sheet == onDrawSheet.sheet then
                        { drawSheet | value = value }

                    else
                        drawSheet

                updatedDrawSheets draw =
                    if draw.id == onDraw.id then
                        { draw | drawSheets = List.map updatedDrawSheet draw.drawSheets }

                    else
                        draw

                updatedDraws draws =
                    List.map updatedDrawSheets draws

                remoteData =
                    case model.remoteData of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.draws }

                        _ ->
                            model.remoteData
            in
            ( { model | remoteData = remoteData }
                |> updateGames
            , Cmd.none
            )

        UpdateDrawLabel onDraw newLabel ->
            let
                updatedDraw draw =
                    if draw.id == onDraw.id then
                        { draw | label = Just newLabel }

                    else
                        draw

                updatedDraws draws =
                    List.map updatedDraw draws

                remoteData =
                    case model.remoteData of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.draws }

                        _ ->
                            model.remoteData
            in
            ( { model | remoteData = remoteData }, Cmd.none )

        UpdateAttendance onDraw newAttendance ->
            let
                updatedDraw draw =
                    if draw.id == onDraw.id then
                        case String.toInt newAttendance of
                            Just attendance ->
                                { draw | attendance = Just attendance }

                            Nothing ->
                                draw

                    else
                        draw

                updatedDraws draws =
                    List.map updatedDraw draws

                remoteData =
                    case model.remoteData of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.draws }

                        _ ->
                            model.remoteData
            in
            ( { model | remoteData = remoteData }, Cmd.none )

        SaveData ->
            let
                updatedDrawSheet games drawSheet =
                    case List.Extra.find (.name >> (==) drawSheet.value) games of
                        Just game ->
                            { drawSheet | gameId = Just game.id }

                        Nothing ->
                            { drawSheet | gameId = Nothing, value = "" }

                updatedDraw games draw =
                    { draw | drawSheets = List.map (updatedDrawSheet games) draw.drawSheets }

                updatedDraws games draws =
                    List.map (updatedDraw games) draws

                remoteData =
                    case model.remoteData of
                        Success decodedData ->
                            Success { decodedData | draws = updatedDraws decodedData.games decodedData.draws }

                        _ ->
                            model.remoteData
            in
            ( { model | remoteData = remoteData }, Cmd.none )

        GotData result ->
            case result of
                Ok data ->
                    ( { model | remoteData = Success data }, Cmd.none )

                Err err ->
                    let
                        errorMessage =
                            case err of
                                Http.BadUrl string ->
                                    "Invalid URL used to fetch the data: " ++ string

                                Http.Timeout ->
                                    "Network timeout when trying to fetch the data."

                                Http.NetworkError ->
                                    "Network timeout when trying to fetch the data."

                                Http.BadStatus int ->
                                    "Invalid response status from server when trying to fetch the data."

                                Http.BadBody string ->
                                    "Invalid response body from server when trying to fetch the data."
                    in
                    ( { model | remoteData = Failure errorMessage }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getData : String -> Cmd Msg
getData url =
    Http.get
        { url = url
        , expect = Http.expectJson GotData dataDecoder
        }
