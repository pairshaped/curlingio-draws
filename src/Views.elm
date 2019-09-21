module Views exposing (view)

import Html exposing (Html, button, datalist, div, input, option, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, id, list, name, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)


view : Model -> Html Msg
view model =
    case model.data of
        NotAsked ->
            text "Initializing..."

        Loading ->
            text "Loading..."

        Failure message ->
            text message

        Success data ->
            viewData data


viewData : Data -> Html Msg
viewData data =
    div [ class "container" ]
        [ viewHeader
        , datalist [ id "games" ] (List.map viewGameOption data.games)
        , viewDrawsContainer data
        , viewFooter
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "row mt-4 mb-4" ]
        [ div [ class "col-8" ] [ text "Instructions" ]
        , div [ class "col-4" ]
            [ div [ class "text-right" ]
                [ button [ class "btn btn-primary", onClick SaveData ] [ text "Save" ]
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
            [ viewSheets data.sheets
            , viewDraws data.draws
            ]
        ]


viewSheets : List String -> Html Msg
viewSheets sheets =
    thead []
        [ tr []
            (List.map viewSheet ((sheets |> (::) "Starts at" |> (::) "Label") ++ [ "Attend" ]))
        ]


viewSheet : String -> Html Msg
viewSheet sheet =
    th [ class "pl-2 pb-2" ] [ text sheet ]


viewDraws : List Draw -> Html Msg
viewDraws draws =
    tbody
        [ class "draws" ]
        (List.map viewDraw draws)


viewDraw : Draw -> Html Msg
viewDraw draw =
    tr
        [ class "draw" ]
        ((List.map (viewDrawSheet draw) draw.drawSheets |> (::) (viewStartsAt draw) |> (::) (viewDrawLabel draw)) ++ [ viewAttendance draw ])


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
                    ++ (case draw.labelChanged of
                            True ->
                                "#ffc107"

                            False ->
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
                    ++ (case draw.startsAtChanged of
                            True ->
                                "#ffc107"

                            False ->
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
                    ++ (case drawSheet.changed of
                            True ->
                                "#ffc107"

                            False ->
                                "#ced4da"
                       )
                )
            , list "games"
            , onInput (SelectedItem draw drawSheet)
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
                    ++ (case draw.attendanceChanged of
                            True ->
                                "#ffc107"

                            False ->
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
    div [ class "ml-1" ]
        [ button [ class "btn btn-primary", onClick AddDraw ] [ text "Add Draw" ]
        ]
