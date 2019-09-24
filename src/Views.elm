module Views exposing (view)

import Html exposing (Html, button, datalist, div, input, option, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, id, list, name, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)


view : Model -> Html Msg
view model =
    case model.data of
        NotAsked ->
            viewNotReady "Initializing..."

        Loading ->
            viewNotReady "Loading..."

        Failure message ->
            viewNotReady message

        Success data ->
            viewData model data


viewNotReady : String -> Html Msg
viewNotReady message =
    div
        [ class "container mt-3" ]
        [ text message ]


viewData : Model -> Data -> Html Msg
viewData model data =
    div [ class "container mt-3" ]
        [ viewHeader model
        , datalist [ id "games" ] (List.map viewGameOption data.games)
        , viewDrawsContainer data
        , viewFooter
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "row mb-4" ]
        [ div [ class "col-8" ] [ text "Instructions" ]
        , div [ class "col-4" ]
            [ div
                [ class "text-right" ]
                [ if model.validated then
                    button [ class "btn btn-primary", disabled (not model.changed), onClick Save ] [ text "Save" ]

                  else
                    button [ class "btn btn-primary", disabled (not model.changed), onClick Validate ] [ text "Validate" ]
                ]
            ]
        ]


viewGameOption : Game -> Html Msg
viewGameOption game =
    option [ disabled game.disabled, value game.name ] []


viewDrawsContainer : Data -> Html Msg
viewDrawsContainer data =
    div
        [ class "table-responsive" ]
        [ table
            [ class "draws-container table table-sm table-borderless table-striped" ]
            [ viewSheets data
            , viewDraws data
            ]
        ]


viewSheets : Data -> Html Msg
viewSheets data =
    let
        addAttendance list =
            if data.hasAttendance then
                list ++ [ "Attend" ]

            else
                list
    in
    thead []
        [ tr []
            (List.map viewSheet
                (data.sheets
                    |> (::) "Starts at"
                    |> (::) "Label"
                    |> addAttendance
                )
            )
        ]


viewSheet : String -> Html Msg
viewSheet sheet =
    th [ class "pl-2 pb-2" ] [ text sheet ]


viewDraws : Data -> Html Msg
viewDraws data =
    tbody
        [ class "draws" ]
        (List.map (viewDraw data.hasAttendance) data.draws)


viewDraw : Bool -> Draw -> Html Msg
viewDraw hasAttendance draw =
    let
        addAttendance list =
            if hasAttendance then
                list ++ [ viewAttendance draw ]

            else
                list
    in
    tr
        [ class "draw" ]
        (List.map (viewDrawSheet draw)
            draw.drawSheets
            |> (::) (viewStartsAt draw)
            |> (::) (viewDrawLabel draw)
            |> addAttendance
        )


viewDrawLabel : Draw -> Html Msg
viewDrawLabel draw =
    td
        [ class "draw_label p-1"
        , style "min-width" "70px"
        , style "max-width" "120px"
        ]
        [ input
            [ class "form-control"
            , style "border"
                ("1px solid "
                    ++ (if draw.labelChanged then
                            "#ffc107"

                        else
                            "#ced4da"
                       )
                )
            , onInput (UpdateDrawLabel draw)
            , value
                (case draw.label of
                    Just label ->
                        label

                    Nothing ->
                        ""
                )
            ]
            []
        ]


viewStartsAt : Draw -> Html Msg
viewStartsAt draw =
    td
        [ class "draw_starts-at p-1"
        , style "min-width" "275px"
        , style "max-width" "275px"
        ]
        [ input
            [ class "form-control"
            , style "border"
                ("1px solid "
                    ++ (if draw.startsAtChanged then
                            "#ffc107"

                        else
                            "#ced4da"
                       )
                )
            , onInput (UpdateDrawStartsAt draw)
            , type_ "datetime-local"
            , value
                (case draw.startsAt of
                    Just startsAt ->
                        startsAt

                    Nothing ->
                        ""
                )
            ]
            []
        ]


viewDrawSheet : Draw -> DrawSheet -> Html Msg
viewDrawSheet draw drawSheet =
    td
        [ class "draw_sheet p-1"
        , style "min-width" "120px"
        , style "max-width" "180px"
        ]
        [ input
            [ class "form-control"
            , style "border"
                ("1px solid "
                    ++ (if drawSheet.problem then
                            "#ff0000"

                        else if drawSheet.changed then
                            "#ffc107"

                        else
                            "#ced4da"
                       )
                )
            , list "games"
            , onInput (SelectedGame draw drawSheet)
            , value drawSheet.value
            ]
            []
        ]


viewAttendance : Draw -> Html Msg
viewAttendance draw =
    td
        [ class "draw_attendance p-1"
        , style "min-width" "70px"
        , style "max-width" "120px"
        ]
        [ input
            [ class "form-control"
            , style "border"
                ("1px solid "
                    ++ (if draw.attendanceChanged then
                            "#ffc107"

                        else
                            "#ced4da"
                       )
                )
            , type_ "number"
            , onInput (UpdateDrawAttendance draw)
            , value
                (case draw.attendance of
                    Just attendance ->
                        String.fromInt attendance

                    Nothing ->
                        ""
                )
            ]
            []
        ]


viewFooter : Html Msg
viewFooter =
    div [ class "footer row" ]
        [ div
            [ class "col" ]
            [ button [ class "btn btn-primary", onClick AddDraw ] [ text "Add draw" ] ]
        , div
            [ class "col text-right" ]
            [ button [ class "btn btn-secondary ml-1", onClick RevertAllChanges ] [ text "Revert all changes" ] ]
        ]
