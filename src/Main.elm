module Main exposing (..)

import Browser
import Html exposing (Html, button, datalist, div, input, p, text)
import Html.Attributes exposing (class, disabled, id, list, name, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODELS


type alias DrawSheet =
    { id : Int
    , value : String
    }


type alias Game =
    { id : Int
    , name : String
    , disabled : Bool
    }


type alias Model =
    { drawSheets : List DrawSheet
    , games : List Game
    }


init =
    { drawSheets =
        List.repeat 128 0
            |> List.indexedMap (\n _ -> DrawSheet (n + 1) "")
    , games =
        List.repeat 128 0
            |> List.indexedMap (\n _ -> Game (n + 1) ("Label for " ++ String.fromInt (n + 1)) False)
    }



-- HELPERS


getGameByName : List Game -> String -> Maybe Game
getGameByName games name =
    List.filter (\o -> o.name == name) games
        |> List.head



-- UPDATE


type Msg
    = SelectedItem DrawSheet String
    | ClearInvalidSelections


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

        ClearInvalidSelections ->
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
    div []
        [ p [] [ text "Used the datalist element to repeat a list of drawSheets that share the same games. When an game is selected in any of the drawSheets it will be disabled in the datalist and can't be selected again." ]
        , datalist [ id "games" ] (List.map viewGame model.games)
        , button [ onClick ClearInvalidSelections ] [ text "Clear invalid selections" ]
        , viewDrawSheets model.drawSheets
        ]


viewGame : Game -> Html Msg
viewGame game =
    Html.game [ disabled game.disabled ] [ text game.name ]


viewDrawSheets : List DrawSheet -> Html Msg
viewDrawSheets drawSheets =
    Keyed.node "div"
        [ class "container" ]
        (List.map viewKeyedDrawSheet drawSheets)


viewKeyedDrawSheet : DrawSheet -> ( String, Html Msg )
viewKeyedDrawSheet drawSheet =
    ( String.fromInt drawSheet.id, lazy viewDrawSheet drawSheet )


viewDrawSheet : DrawSheet -> Html Msg
viewDrawSheet drawSheet =
    input
        [ list "games"
        , name (String.fromInt drawSheet.id)
        , onInput (SelectedItem drawSheet)
        , value drawSheet.value
        ]
        []
