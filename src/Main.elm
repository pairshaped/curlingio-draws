module Main exposing (..)

import Browser
import Html exposing (Html, button, datalist, div, input, option, p, text)
import Html.Attributes exposing (class, disabled, id, list, name, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import List.Extra


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODELS


type alias Draw =
    { id : Int
    , label : Maybe String
    , starts_at : Maybe String
    , attendance : Maybe Int
    , drawSheets : List DrawSheet
    }


type alias DrawSheet =
    { sheet : Int
    , gameId : Maybe Int
    , value : String
    }


type alias Game =
    { id : Int
    , name : String
    , teams : ( Int, Int )
    , disabled : Bool
    }


type alias Model =
    { numberOfSheets : Int
    , games : List Game
    , draws : List Draw
    }


init =
    { numberOfSheets = 4
    , games = []
    , draws = []
    }



-- UPDATE


type Msg
    = SelectedItem Draw DrawSheet String
    | UpdateDrawLabel Draw String
    | UpdateAttendance Draw String
    | Save


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


update : Msg -> Model -> Model
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
            { model | draws = updatedDraws }
                |> updateGames

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
            { model | draws = updateDraws }

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
            { model | draws = updateDraws }

        Save ->
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
            { model | draws = updatedDraws }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewHeader
        , datalist [ id "games" ] (List.map viewGameOption model.games)
        , viewDraws model.draws
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "row mt-4 mb-4" ]
        [ div [ class "col-8" ] [ text "Instructions" ]
        , div [ class "col-4" ]
            [ div [ class "text-right" ]
                [ button [ class "btn btn-primary", onClick Save ] [ text "Save" ]
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
