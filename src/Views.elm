module Views exposing (view)

import Helpers exposing (..)
import Html exposing (Html, button, datalist, div, em, input, option, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, id, list, min, name, required, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import RemoteData exposing (RemoteData(..))
import Types exposing (..)


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
    div [ class "mt-3" ]
        [ datalist [ id "games" ] (List.map (viewGameOption schedule.teams) schedule.games)
        , viewDrawsContainer model schedule
        , viewFooter model
        ]


viewGameOption : List Team -> Game -> Html Msg
viewGameOption teams game =
    option [ disabled game.disabled, value (gameName game) ] []


viewDrawsContainer : Model -> Schedule -> Html Msg
viewDrawsContainer model schedule =
    div
        [ class "table-responsive" ]
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
        (List.indexedMap (viewDraw model schedule.settings) schedule.draws)


viewDraw : Model -> Settings -> Int -> Draw -> Html Msg
viewDraw model settings index draw =
    let
        addAttendance list =
            if settings.hasAttendance then
                list ++ [ viewAttendance index draw ]

            else
                list

        addDelete list =
            list ++ [ viewDelete model index ]
    in
    tr []
        (List.map (viewDrawSheet index)
            draw.drawSheets
            |> (::) (viewStartsAt settings index draw)
            |> (::) (viewDrawLabel index draw)
            |> addAttendance
            |> addDelete
        )


viewDrawLabel : Int -> Draw -> Html Msg
viewDrawLabel index draw =
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
            , required True
            , onInput (UpdateDrawLabel index)
            , value draw.label.value
            ]
            []
        ]


viewStartsAt : Settings -> Int -> Draw -> Html Msg
viewStartsAt settings index draw =
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
            , onInput (UpdateDrawStartsAt index)
            , type_ "datetime-local"
            , Html.Attributes.min settings.minDateTime
            , Html.Attributes.max settings.maxDateTime
            , required True
            , value draw.startsAt.value
            ]
            []
        ]


viewDrawSheet : Int -> DrawSheet -> Html Msg
viewDrawSheet index drawSheet =
    td []
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
            , list "games"
            , onInput (SelectedGame index drawSheet)
            , value drawSheet.value
            ]
            []
        ]


viewAttendance : Int -> Draw -> Html Msg
viewAttendance index draw =
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
            , required True
            , Html.Attributes.min "0"
            , Html.Attributes.max "99999"
            , onInput (UpdateDrawAttendance index)
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
viewDelete model index =
    td
        [ class "text-right", style "padding-top" "7px" ]
        [ button
            [ class "btn btn-sm btn-secondary", style "min-width" "28px", disabled (model.savedDraws == Loading), onClick (DeleteDraw index) ]
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
