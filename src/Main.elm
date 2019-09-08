module Main exposing (..)

import Browser
import Html exposing (Html, button, datalist, div, input, option, p, text)
import Html.Attributes exposing (class, disabled, id, list, name, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODELS


type alias Draw =
    { id : Maybe Int
    , index : Int
    , starts_at : Maybe Date
    , attendance : Maybe Int
    }


type alias DrawSheet =
    { id : Maybe Int
    , drawIndex : Int
    , sheetIndex : Int
    , value : Maybe String
    }


type alias Game =
    { id : Int
    , name : String
    , disabled : Bool
    }


type alias Model =
    { draws : List Draw
    , drawSheets : List DrawSheet
    , games : List Game
    , numberOfSheets : Int
    }


init =
    { draws =
        List.repeat 12 0
            |> List.indexedMap (\n _ -> DrawSheet Nothing n Nothing Nothing)
    , drawSheets =
        List.repeat 12 0
            |> List.repeat 4 0
            |> List.indexedMap (\n _ -> DrawSheet Nothing n "")
    , games =
        List.repeat 128 0
            |> List.indexedMap (\n _ -> Game (n + 1) ("Label for " ++ String.fromInt (n + 1)) False)
    , numberOfSheets = 4
    }



-- HELPERS


getGameByName : List Game -> String -> Maybe Game
getGameByName games name =
    List.filter (\o -> o.name == name) games
        |> List.head



-- UPDATE


type Msg
    = SelectedItem DrawSheet String
    | Save


updateGames : Model -> Model
updateGames model =
    let
        -- Loop through the games setting each to disabled if a matching drawSheet value is the same as it's name
        gameIsSelected : Game -> Bool
        gameIsSelected game =
            List.any (\d -> d.value == game.name) model.drawSheets

        updateGame : Game -> Game
        updateGame game =
            { game | disabled = gameIsSelected game }
    in
    { model | games = List.map updateGame model.games }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectedItem onDropDown value ->
            let
                updateDropDown drawSheet =
                    if drawSheet.id == onDropDown.id then
                        { drawSheet | value = value }

                    else
                        drawSheet

                updateDropDowns =
                    List.map updateDropDown model.drawSheets
            in
            { model | drawSheets = updateDropDowns }
                |> updateGames

        Save ->
            let
                validDrawSheet drawSheet =
                    if List.any (\o -> o.name == drawSheet.value) model.games then
                        drawSheet

                    else
                        { drawSheet | value = "" }

                validDrawSheets =
                    List.map validDrawSheet model.drawSheets
            in
            { model | drawSheets = validDrawSheets }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewHeader
        , datalist [ id "games" ] (List.map viewGameOption model.games)
        , viewDraws model.drawSheets
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


viewDraws : List DrawSheet -> Html Msg
viewDraws drawSheets =
    div [ class "row" ]
        [ div [ class "col-12" ]
            [ viewDrawSheets drawSheets
            ]
        ]


viewDrawSheets : List DrawSheet -> Html Msg
viewDrawSheets drawSheets =
    Keyed.node "div"
        [ class "d-flex justify-content-between" ]
        (List.map viewKeyedDrawSheet drawSheets)


viewKeyedDrawSheet : DrawSheet -> ( String, Html Msg )
viewKeyedDrawSheet drawSheet =
    ( String.fromInt drawSheet.id, lazy viewDrawSheet drawSheet )


viewDrawSheet : DrawSheet -> Html Msg
viewDrawSheet drawSheet =
    input
        [ class "m-1 p-1"
        , list "games"
        , name (String.fromInt drawSheet.id)
        , onInput (SelectedItem drawSheet)
        , value drawSheet.value
        ]
        []
