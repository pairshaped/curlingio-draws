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
        , viewFooter model
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
        (List.indexedMap (viewDraw data.hasAttendance) data.draws)


viewDraw : Bool -> Int -> Draw -> Html Msg
viewDraw hasAttendance index draw =
    let
        addAttendance list =
            if hasAttendance then
                list ++ [ viewAttendance index draw ]

            else
                list
    in
    tr
        [ class "draw" ]
        (List.map (viewDrawSheet index)
            draw.drawSheets
            |> (::) (viewStartsAt index draw)
            |> (::) (viewDrawLabel index draw)
            |> addAttendance
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
            , onInput (UpdateDrawLabel index)
            , value
                (case draw.label.value of
                    Just value ->
                        value

                    Nothing ->
                        ""
                )
            ]
            []
        ]


viewStartsAt : Int -> Draw -> Html Msg
viewStartsAt index draw =
    td
        [ class "draw_starts-at p-1"
        , style "min-width" "275px"
        , style "max-width" "275px"
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
            , value
                (case draw.startsAt.value of
                    Just value ->
                        value

                    Nothing ->
                        ""
                )
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
            , onInput (UpdateDrawAttendance index)
            , value
                (case draw.attendance.value of
                    Just value ->
                        String.fromInt value

                    Nothing ->
                        ""
                )
            ]
            []
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    div [ class "footer row" ]
        [ div
            [ class "col" ]
            [ button [ class "btn btn-primary", onClick AddDraw ] [ text "Add draw" ] ]
        , div
            [ class "col text-right" ]
            [ button [ class "btn btn-secondary ml-1", disabled (not model.changed), onClick RevertAllChanges ] [ text "Revert all changes" ] ]
        ]
