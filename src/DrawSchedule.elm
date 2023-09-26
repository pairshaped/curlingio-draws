module DrawSchedule exposing (..)

import Browser
import Html exposing (Html, a, button, datalist, div, em, input, option, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, id, min, name, style, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html.Events.Extra exposing (onClickPreventDefaultAndStopPropagation)
import Http
import Json.Decode as Decode exposing (Decoder, bool, index, int, list, map2, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http



-- MODEL


type alias Model =
    { flags : Flags
    , schedule : WebData Schedule
    , changed : Bool
    , validated : Bool
    , savedDraws : WebData SavedDraws
    , selectedDrawSheet : Maybe SelectableDrawSheet
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
    { hasAttendance : Bool
    , minDateTime : String
    , maxDateTime : String
    }


type alias Team =
    { id : Int
    , name : String
    }


type alias Game =
    { id : String
    , name : String
    , stageName : String
    , teamIds : ( Maybe Int, Maybe Int )
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
    , gameId : Maybe String
    , value : String
    , changed : Bool
    , valid : Bool
    }


type alias SelectableDrawSheet =
    { drawIndex : Int
    , drawSheet : DrawSheet
    }



-- DECODERS


decodeSchedule : Decoder Schedule
decodeSchedule =
    Decode.succeed Schedule
        |> required "settings" decodeSettings
        |> required "sheets" (list string)
        |> required "teams" (list decodeTeam)
        |> required "games" (list decodeGame)
        |> required "draws" (list decodeDraw)


decodeSettings : Decoder Settings
decodeSettings =
    Decode.succeed Settings
        |> optional "has_attendance" bool False
        |> required "min_datetime" string
        |> required "max_datetime" string


decodeTeam : Decoder Team
decodeTeam =
    Decode.succeed Team
        |> required "id" int
        |> required "name" string


decodeGame : Decoder Game
decodeGame =
    Decode.succeed Game
        |> required "id" string
        |> required "name" string
        |> required "stage_name" string
        |> required "team_ids" decodeTeamIds


decodeTeamIds : Decoder ( Maybe Int, Maybe Int )
decodeTeamIds =
    map2 Tuple.pair (Decode.maybe (index 0 int)) (Decode.maybe (index 1 int))


decodeSavedDraws : Decoder SavedDraws
decodeSavedDraws =
    Decode.succeed SavedDraws
        |> required "draws" (list decodeDraw)


decodeDraw : Decoder Draw
decodeDraw =
    Decode.succeed Draw
        |> required "id" (nullable int)
        |> required "label" (string |> Decode.map (\val -> DrawLabel val False True))
        |> required "starts_at" (string |> Decode.map (\val -> DrawStartsAt val False True))
        |> required "attendance" (nullable int |> Decode.map (\val -> DrawAttendance val False True))
        |> required "draw_sheets" (list decodeDrawSheet)


decodeDrawSheet : Decoder DrawSheet
decodeDrawSheet =
    Decode.succeed DrawSheet
        |> required "sheet" int
        |> required "game_id" (nullable string)
        |> optional "value" string ""
        |> hardcoded False
        |> hardcoded True



-- ENCODERS


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
        , ( "game_id", maybe Encode.string drawSheet.gameId )
        ]


encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe encoder maybe =
    case maybe of
        Just value ->
            encoder value

        Nothing ->
            Encode.null



-- HELPERS


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags NotAsked False True NotAsked Nothing, getSchedule flags.url )


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


selectableGames : List Game -> List Draw -> Int -> List Game
selectableGames games draws drawIndex =
    let
        teamIdsInDraw =
            let
                drawGameIds draw =
                    List.map (\ds -> ds.gameId) draw.drawSheets
                        |> List.filterMap identity
            in
            case List.Extra.getAt drawIndex draws of
                Just draw ->
                    games
                        |> List.filter (\g -> List.member g.id (drawGameIds draw))
                        |> List.map (\g -> [ Tuple.first g.teamIds, Tuple.second g.teamIds ])
                        |> List.foldr (++) []
                        |> List.filterMap identity

                Nothing ->
                    []

        scheduledGameIds =
            List.map (\d -> d.drawSheets) draws
                |> List.foldr (++) []
                |> List.map (\ds -> ds.gameId)
                |> List.filterMap identity

        unscheduledGames =
            let
                notScheduled game =
                    not (List.member game.id scheduledGameIds)
            in
            List.filter notScheduled games

        -- exclude all games that have a team id that's in the draws team ids.
        gameHasNoTeamsInDraw game =
            teamIdsInDraw
                |> List.any (\id -> (Tuple.first game.teamIds == Just id) || (Tuple.second game.teamIds == Just id))
                |> not
    in
    -- Exclude games with a team that has already been selected in the draw.
    unscheduledGames
        |> List.filter gameHasNoTeamsInDraw


getSchedule : String -> Cmd Msg
getSchedule url =
    RemoteData.Http.get url GotSchedule decodeSchedule


patchDraws : String -> List Draw -> Cmd Msg
patchDraws url draws =
    RemoteData.Http.patch url PatchedDraws decodeSavedDraws (encodeDraws draws)


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


findGameByName : List Game -> String -> Maybe Game
findGameByName games name =
    List.Extra.find (\game -> gameName game == name) games


validateDrawSheets : Schedule -> Draw -> Draw
validateDrawSheets schedule draw =
    let
        validateDrawSheet drawSheet =
            case findGameByName schedule.games drawSheet.value of
                Just game ->
                    { drawSheet | gameId = Just game.id, valid = True }

                Nothing ->
                    if drawSheet.value == "" then
                        { drawSheet | gameId = Nothing, valid = True }

                    else
                        { drawSheet | gameId = Nothing, valid = False }

        validatedDrawSheets =
            List.map validateDrawSheet draw.drawSheets
    in
    { draw | drawSheets = validatedDrawSheets }


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



-- UPDATE


type Msg
    = GotSchedule (WebData Schedule)
    | PatchedDraws (WebData SavedDraws)
    | DiscardChanges
    | UpdateDrawLabel Int String
    | UpdateDrawStartsAt Int String
    | UpdateDrawAttendance Int String
    | FocusedDrawSheet SelectableDrawSheet
    | BlurredDrawSheet
    | SelectedGame SelectableDrawSheet (Maybe Game)
    | AddDraw
    | DeleteDraw Int
    | Save


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSchedule schedule ->
            ( { model | schedule = schedule }
                |> populateDrawSheetValues
            , Cmd.none
            )

        PatchedDraws draws ->
            ( { model | schedule = Loading, savedDraws = draws, changed = False }
            , getSchedule model.flags.url
            )

        DiscardChanges ->
            ( { model | schedule = Loading, changed = False, validated = True }, getSchedule model.flags.url )

        UpdateDrawLabel drawIndex newLabel ->
            let
                updatedDrawLabel draws label =
                    { label | value = newLabel, changed = True, valid = drawLabelIsValid draws newLabel }

                updatedDraw draws draw =
                    { draw | label = updatedDrawLabel draws draw.label }

                updatedDraws draws =
                    List.Extra.updateAt drawIndex (\draw -> updatedDraw draws draw) draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule.draws }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, changed = True, validated = False }, Cmd.none )

        UpdateDrawStartsAt drawIndex newStartsAt ->
            let
                updatedDrawStartsAt draws startsAt =
                    { startsAt | value = newStartsAt, changed = True, valid = drawStartsAtIsValid draws newStartsAt }

                updatedDraw draws draw =
                    { draw | startsAt = updatedDrawStartsAt draws draw.startsAt }

                updatedDraws draws =
                    List.Extra.updateAt drawIndex (\draw -> updatedDraw draws draw) draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule.draws }

                        _ ->
                            model.schedule
            in
            ( { model | schedule = updatedSchedule, changed = True, validated = False }, Cmd.none )

        UpdateDrawAttendance drawIndex newAttendance ->
            let
                updatedDrawAttendance attendance =
                    { attendance | value = String.toInt newAttendance, changed = True, valid = drawAttendanceIsValid newAttendance }

                updatedDraw draw =
                    { draw | attendance = updatedDrawAttendance draw.attendance }

                updatedDraws draws =
                    List.Extra.updateAt drawIndex (\draw -> updatedDraw draw) draws

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

        FocusedDrawSheet selectedDrawSheet ->
            ( { model | selectedDrawSheet = Just selectedDrawSheet }
            , Cmd.none
            )

        BlurredDrawSheet ->
            ( { model | selectedDrawSheet = Nothing }
            , Cmd.none
            )

        SelectedGame { drawIndex, drawSheet } game ->
            let
                updatedDrawSheet schedule draw onDrawSheet =
                    if onDrawSheet.sheet == drawSheet.sheet then
                        case game of
                            Just game_ ->
                                { onDrawSheet
                                    | value = gameName game_
                                    , gameId = Just game_.id
                                    , changed = True
                                }

                            Nothing ->
                                { onDrawSheet
                                    | value = ""
                                    , gameId = Nothing
                                    , changed = True
                                }

                    else
                        onDrawSheet

                updatedDraw schedule draw =
                    { draw | drawSheets = List.map (updatedDrawSheet schedule draw) draw.drawSheets }
                        |> validateDrawSheets schedule

                updatedDraws schedule =
                    List.Extra.updateAt drawIndex (\draw -> updatedDraw schedule draw) schedule.draws

                updatedSchedule =
                    case model.schedule of
                        Success decodedSchedule ->
                            Success { decodedSchedule | draws = updatedDraws decodedSchedule }

                        _ ->
                            model.schedule
            in
            ( { model
                | schedule = updatedSchedule
                , selectedDrawSheet = Nothing
                , changed = True
                , validated = False
              }
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



-- VIEW


view : Model -> Html Msg
view model =
    case model.schedule of
        NotAsked ->
            viewNotReady "Initializing..."

        Loading ->
            viewNotReady "Loading..."

        Failure error ->
            let
                errorMessage =
                    case error of
                        Http.BadUrl string ->
                            "Bad URL used to fetch schedule: " ++ string

                        Http.Timeout ->
                            "Network timeout when trying to fetch schedule."

                        Http.NetworkError ->
                            "Network error when trying to fetch schedule."

                        Http.BadStatus int ->
                            "Bad status response from server when trying to fetch schedule."

                        Http.BadBody string ->
                            "Bad body response from server when trying to fetch schedule: " ++ string
            in
            viewNotReady errorMessage

        Success schedule ->
            viewSchedule model schedule


viewNotReady : String -> Html Msg
viewNotReady message =
    div
        [ class "mt-3" ]
        [ text message ]


viewSchedule : Model -> Schedule -> Html Msg
viewSchedule model schedule =
    div
        [ class "container-fluid pt-3 pb-3"
        , onClick BlurredDrawSheet
        ]
        [ viewDrawsContainer model schedule
        , viewFooter model
        ]


viewDrawsContainer : Model -> Schedule -> Html Msg
viewDrawsContainer model schedule =
    div [ class "table-responsive mb-2" ]
        [ table
            [ class "table table-sm table-borderless table-striped"
            , style "table-layout" "fixed"
            ]
            [ viewSheets schedule
            , viewDraws model schedule
            ]
        ]


viewSheets : Schedule -> Html Msg
viewSheets schedule =
    let
        labelHeader list =
            th [ style "width" "70px" ] [ text "Label" ] :: list

        startsAtHeader list =
            th [ style "width" "265px" ] [ text "Starts at" ] :: list

        sheetHeader sheet =
            th [ style "width" "150px" ] [ text sheet ]

        attendanceHeader list =
            if schedule.settings.hasAttendance then
                list ++ [ th [ style "width" "80px" ] [ text "Attend" ] ]

            else
                list

        deleteHeader list =
            list ++ [ th [ style "width" "35px" ] [ text "" ] ]
    in
    thead []
        [ tr []
            (List.map sheetHeader schedule.sheets
                |> startsAtHeader
                |> labelHeader
                |> attendanceHeader
                |> deleteHeader
            )
        ]


viewDraws : Model -> Schedule -> Html Msg
viewDraws model schedule =
    tbody []
        (List.indexedMap (viewDraw model schedule.settings schedule) schedule.draws)


viewDraw : Model -> Settings -> Schedule -> Int -> Draw -> Html Msg
viewDraw model settings { games, draws } drawIndex draw =
    let
        addAttendance drawSheetViews =
            if settings.hasAttendance then
                drawSheetViews ++ [ viewAttendance drawIndex draw ]

            else
                drawSheetViews

        addDelete drawSheetViews =
            drawSheetViews ++ [ viewDelete model drawIndex ]
    in
    tr []
        (List.map (viewDrawSheet games draws model.selectedDrawSheet drawIndex) draw.drawSheets
            |> (::) (viewStartsAt settings drawIndex draw)
            |> (::) (viewDrawLabel drawIndex draw)
            |> addAttendance
            |> addDelete
        )


viewDrawLabel : Int -> Draw -> Html Msg
viewDrawLabel drawIndex draw =
    td []
        [ input
            [ class "form-control"
            , style "border"
                ("1px solid "
                    ++ (if not draw.label.valid then
                            "#ff0000"

                        else if draw.label.changed then
                            "#ffc107"

                        else
                            "#ced4da"
                       )
                )
            , Html.Attributes.required True
            , onInput (UpdateDrawLabel drawIndex)
            , value draw.label.value
            ]
            []
        ]


viewStartsAt : Settings -> Int -> Draw -> Html Msg
viewStartsAt settings drawIndex draw =
    td []
        [ input
            [ class "form-control"
            , style "border"
                ("1px solid "
                    ++ (if not draw.startsAt.valid then
                            "#ff0000"

                        else if draw.startsAt.changed then
                            "#ffc107"

                        else
                            "#ced4da"
                       )
                )
            , onInput (UpdateDrawStartsAt drawIndex)
            , type_ "datetime-local"
            , Html.Attributes.min settings.minDateTime
            , Html.Attributes.max settings.maxDateTime
            , Html.Attributes.required True
            , value draw.startsAt.value
            ]
            []
        ]


viewDrawSheet : List Game -> List Draw -> Maybe SelectableDrawSheet -> Int -> DrawSheet -> Html Msg
viewDrawSheet games draws selectedDrawSheet drawIndex drawSheet =
    td [ id ("drawIndex" ++ String.fromInt drawIndex ++ "-sheet" ++ String.fromInt drawSheet.sheet), style "position" "relative" ]
        [ div [ style "overflow" "hidden" ]
            [ input
                [ class "form-control"
                , style "border"
                    ("1px solid "
                        ++ (if not drawSheet.valid then
                                "#ff0000"

                            else if drawSheet.changed then
                                "#ffc107"

                            else
                                "#ced4da"
                           )
                    )
                , Html.Attributes.list "games"
                , onFocus (FocusedDrawSheet (SelectableDrawSheet drawIndex drawSheet))
                , onClickPreventDefaultAndStopPropagation (FocusedDrawSheet (SelectableDrawSheet drawIndex drawSheet))
                , value drawSheet.value
                ]
                []
            , case selectedDrawSheet of
                Just selected ->
                    if selected.drawIndex == drawIndex && selected.drawSheet.sheet == drawSheet.sheet then
                        viewSelectableGames (selectableGames games draws drawIndex) selected

                    else
                        text ""

                Nothing ->
                    text ""
            ]
        ]


viewSelectableGames : List Game -> SelectableDrawSheet -> Html Msg
viewSelectableGames games selectedDrawSheet =
    -- The games in the datalist depends on which draw we're trying to assign a game to.
    -- if we aren't trying to assign a game, then it can be empty.
    let
        viewEmptyOption =
            div
                [ class "draws__selectable-game"
                , onClickPreventDefaultAndStopPropagation (SelectedGame selectedDrawSheet Nothing)
                ]
                [ text "-" ]

        viewGameOption game =
            div
                [ class "draws__selectable-game"
                , onClickPreventDefaultAndStopPropagation (SelectedGame selectedDrawSheet (Just game))
                ]
                [ text (gameName game) ]
    in
    div
        [ class "draws__selectable-games" ]
        ([ viewEmptyOption ]
            ++ List.map viewGameOption games
        )


viewAttendance : Int -> Draw -> Html Msg
viewAttendance drawIndex draw =
    td []
        [ input
            [ class "form-control"
            , style "border"
                ("1px solid "
                    ++ (if not draw.attendance.valid then
                            "#ff0000"

                        else if draw.attendance.changed then
                            "#ffc107"

                        else
                            "#ced4da"
                       )
                )
            , type_ "number"
            , Html.Attributes.required True
            , Html.Attributes.min "0"
            , Html.Attributes.max "99999"
            , onInput (UpdateDrawAttendance drawIndex)
            , value
                (case draw.attendance.value of
                    Just value ->
                        String.fromInt value

                    Nothing ->
                        "0"
                )
            ]
            []
        ]


viewDelete : Model -> Int -> Html Msg
viewDelete model drawIndex =
    td
        [ class "text-right", style "padding-top" "7px" ]
        [ button
            [ class "btn btn-sm btn-secondary", style "min-width" "28px", disabled (model.savedDraws == Loading), onClick (DeleteDraw drawIndex) ]
            [ text "X" ]
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    div [ class "row mb-3" ]
        [ div
            [ class "col d-flex" ]
            [ div
                [ class "mr-1" ]
                [ button [ class "btn btn-primary", style "min-width" "90px", disabled (model.savedDraws == Loading || not model.changed || not (validForSave model)), onClick Save ] [ text "Save" ]
                ]
            , div
                [ class "mr-1" ]
                [ button [ class "btn btn-secondary", style "min-width" "90px", disabled (model.savedDraws == Loading || not model.changed), onClick DiscardChanges ] [ text "Reset" ] ]
            ]
        , div
            [ class "col text-right" ]
            [ button [ class "btn btn-primary", style "min-width" "90px", disabled (model.savedDraws == Loading), onClick AddDraw ] [ text "Add Draw" ] ]
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
