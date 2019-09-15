module Views exposing (view)

import Html exposing (Html, button, datalist, div, input, option, p, text)
import Html.Attributes exposing (class, disabled, id, list, name, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Types exposing (..)


view : Model -> Html Msg
view model =
    case model.remoteData of
        Failure message ->
            text message

        Loading ->
            text "Loading..."

        Success decodedData ->
            div [ class "container" ]
                [ viewHeader
                , datalist [ id "games" ] (List.map viewGameOption decodedData.games)
                , viewDraws decodedData.draws
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
    option [ disabled game.disabled ] [ text game.name ]


viewDraws : List Draw -> Html Msg
viewDraws draws =
    Keyed.node "div"
        []
        (List.map viewKeyedDraw draws)


viewKeyedDraw : Draw -> ( String, Html Msg )
viewKeyedDraw draw =
    ( String.fromInt draw.id, lazy viewDraw draw )


viewDraw : Draw -> Html Msg
viewDraw draw =
    Keyed.node "div"
        [ class "d-flex justify-content-between" ]
        (List.map (viewKeyedDrawSheet draw) draw.drawSheets)


viewKeyedDrawSheet : Draw -> DrawSheet -> ( String, Html Msg )
viewKeyedDrawSheet draw drawSheet =
    ( String.fromInt draw.id ++ String.fromInt drawSheet.sheet, lazy (viewDrawSheet draw) drawSheet )


viewDrawSheet : Draw -> DrawSheet -> Html Msg
viewDrawSheet draw drawSheet =
    input
        [ class "m-1 p-1"
        , list "games"
        , onInput (SelectedItem draw drawSheet)
        , value drawSheet.value
        ]
        []
