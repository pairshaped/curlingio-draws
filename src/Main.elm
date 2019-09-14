module Main exposing (..)

import Browser
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

        gameIsSelected game =
            List.map (gameIsSelectedInDraw game) model.draws
                |> List.member True

        updateGame game =
            { game | disabled = gameIsSelected game }
    in
    { model | games = List.map updateGame model.games }


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

                updatedDraws =
                    List.map updatedDrawSheets model.draws
            in
            ( { model | draws = updatedDraws }
                |> updateGames
            , Cmd.none
            )

        UpdateDrawLabel onDraw newLabel ->
            let
                updateDraw draw =
                    if draw.id == onDraw.id then
                        { draw | label = Just newLabel }

                    else
                        draw

                updateDraws =
                    List.map updateDraw model.draws
            in
            ( { model | draws = updateDraws }, Cmd.none )

        UpdateAttendance onDraw newAttendance ->
            let
                updateDraw draw =
                    if draw.id == onDraw.id then
                        case String.toInt newAttendance of
                            Just attendance ->
                                { draw | attendance = Just attendance }

                            Nothing ->
                                draw

                    else
                        draw

                updateDraws =
                    List.map updateDraw model.draws
            in
            ( { model | draws = updateDraws }, Cmd.none )

        SaveData ->
            let
                updatedDrawSheet drawSheet =
                    case List.Extra.find (.name >> (==) drawSheet.value) model.games of
                        Just game ->
                            { drawSheet | gameId = Just game.id }

                        Nothing ->
                            { drawSheet | gameId = Nothing, value = "" }

                updatedDraw draw =
                    { draw | drawSheets = List.map updatedDrawSheet draw.drawSheets }

                updatedDraws =
                    List.map updatedDraw model.draws
            in
            ( { model | draws = updatedDraws }, Cmd.none )

        GotData result ->
            case result of
                Ok remoteData ->
                    ( { model | remoteData = Success remoteData }, Cmd.none )

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
getData url section =
    Http.get
        { url = url
        , expect = Http.expectJson GotData dataDecoder
        }
