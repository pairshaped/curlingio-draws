module Views exposing (view)

import Html exposing (Html, button, datalist, div, input, option, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, id, list, name, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
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


viewDrawsContainer : Data -> Html Msg
viewDrawsContainer data =
    table [ class "draws-container table table-sm table-borderless table-striped" ]
        [ viewSheets data.sheets
        , viewDraws data.draws
        ]


viewSheets : List String -> Html Msg
viewSheets sheets =
    thead []
        [ tr [] (List.map viewSheet sheets)
        ]


viewSheet : String -> Html Msg
viewSheet sheet =
    th [ class "pl-2 pb-2" ] [ text sheet ]


viewGameOption : Game -> Html Msg
viewGameOption game =
    option [ disabled game.disabled, value game.name ] []


viewDraws : List Draw -> Html Msg
viewDraws draws =
    Keyed.node "tbody"
        [ class "draws" ]
        (List.map viewKeyedDraw draws)


viewKeyedDraw : Draw -> ( String, Html Msg )
viewKeyedDraw draw =
    ( String.fromInt draw.id, lazy viewDraw draw )


viewDraw : Draw -> Html Msg
viewDraw draw =
    Keyed.node "tr"
        [ class "draw" ]
        (List.map (viewKeyedDrawSheet draw) draw.drawSheets)


viewKeyedDrawSheet : Draw -> DrawSheet -> ( String, Html Msg )
viewKeyedDrawSheet draw drawSheet =
    ( String.fromInt draw.id ++ String.fromInt drawSheet.sheet, lazy (viewDrawSheet draw) drawSheet )


viewDrawSheet : Draw -> DrawSheet -> Html Msg
viewDrawSheet draw drawSheet =
    td [ class "draw-sheet p-2" ]
        [ input
            [ class "form-control"
            , list "games"
            , onInput (SelectedItem draw drawSheet)
            , value drawSheet.value
            ]
            []
        ]
