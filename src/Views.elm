module Views exposing (view)

import Helpers exposing (..)
import Html exposing (Html, button, datalist, div, em, input, option, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, id, list, max, min, name, required, style, type_, value)
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
        [ class "container mt-3" ]
        [ text message ]


viewSchedule : Model -> Schedule -> Html Msg
viewSchedule model schedule =
    div [ class "container mt-3" ]
        [ viewHeader model
        , datalist [ id "games" ] (List.map (viewGameOption schedule.teams) schedule.games)
        , viewDrawsContainer schedule
        , viewFooter model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "mb-4 d-flex" ]
        [ div
            [ class "mr-3" ]
            [ em
                []
                [ text "Select the games to be played in each draw. If an input is highlighted in red, it's invalid and needs to be fixed. If an input in highlighted in yellow, it's been modified but not yet saved." ]
            ]
        , div []
            [ div
                [ class "text-right" ]
                [ button [ class "btn btn-primary", disabled (not model.changed || not (validForSave model)), onClick Save ] [ text "Save" ]
                ]
            ]
        ]


viewGameOption : List Team -> Game -> Html Msg
viewGameOption teams game =
    option [ disabled game.disabled, value (nameOfGame teams game) ] []


viewDrawsContainer : Schedule -> Html Msg
viewDrawsContainer schedule =
    div
        [ class "table-responsive" ]
        [ table
            [ class "draws-container table table-sm table-borderless table-striped" ]
            [ viewSheets schedule
            , viewDraws schedule
            ]
        ]


viewSheets : Schedule -> Html Msg
viewSheets schedule =
    let
        addAttendance list =
            if schedule.settings.hasAttendance then
                list ++ [ "Attend" ]

            else
                list

        addDelete list =
            list ++ [ "" ]
    in
    thead []
        [ tr []
            (List.map viewSheet
                (schedule.sheets
                    |> (::) "Starts at"
                    |> (::) "Label"
                    |> addAttendance
                    |> addDelete
                )
            )
        ]


viewSheet : String -> Html Msg
viewSheet sheet =
    th [ class "pl-2 pb-2" ] [ text sheet ]


viewDraws : Schedule -> Html Msg
viewDraws schedule =
    tbody
        [ class "draws" ]
        (List.indexedMap (viewDraw schedule.settings.hasAttendance) schedule.draws)


viewDraw : Bool -> Int -> Draw -> Html Msg
viewDraw hasAttendance index draw =
    let
        addAttendance list =
            if hasAttendance then
                list ++ [ viewAttendance index draw ]

            else
                list

        addDelete list =
            list ++ [ viewDelete index ]
    in
    tr
        [ class "draw" ]
        (List.map (viewDrawSheet index)
            draw.drawSheets
            |> (::) (viewStartsAt index draw)
            |> (::) (viewDrawLabel index draw)
            |> addAttendance
            |> addDelete
        )


viewDrawLabel : Int -> Draw -> Html Msg
viewDrawLabel index draw =
    td
        [ class "draw_label p-1"
        , style "min-width" "70px"
        , style "max-width" "120px"
        ]
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


viewStartsAt : Int -> Draw -> Html Msg
viewStartsAt index draw =
    td
        [ class "draw_starts-at p-1"
        , style "min-width" "265px"
        , style "max-width" "265px"
        ]
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
            , required True
            , value draw.startsAt.value
            ]
            []
        ]


viewDrawSheet : Int -> DrawSheet -> Html Msg
viewDrawSheet index drawSheet =
    td
        [ class "draw_sheet p-1"
        , style "min-width" "120px"
        , style "max-width" "180px"
        ]
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
    td
        [ class "draw_attendance p-1"
        , style "min-width" "70px"
        , style "max-width" "120px"
        ]
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
            , min "0"
            , max "99999"
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


viewDelete : Int -> Html Msg
viewDelete index =
    td
        [ class "draw_delete p-2 text-right"
        , style "min-width" "40px"
        , style "max-width" "40px"
        ]
        [ button
            [ class "btn btn-sm btn-danger", onClick (DeleteDraw index) ]
            [ text "X" ]
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    div [ class "footer row" ]
        [ div
            [ class "col" ]
            [ button [ class "btn btn-primary", onClick AddDraw ] [ text "Add draw" ] ]
        , div
            [ class "col text-right" ]
            [ button [ class "btn btn-secondary ml-1", disabled (not model.changed), onClick DiscardChanges ] [ text "Discard changes" ] ]
        ]
