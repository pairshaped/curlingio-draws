module Selects exposing (..)

import Browser
import Html exposing (Html, div, option, p, select, text)
import Html.Attributes exposing (class, disabled, name, selected, value)
import Html.Events exposing (onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODELS


type alias DropDown =
    { id : Int
    , selectedOption : Maybe Option
    }


type alias Option =
    { id : Int
    , name : String
    , disabled : Bool
    }


type alias Model =
    { dropDowns : List DropDown
    , options : List Option
    }


init =
    { dropDowns =
        List.repeat 256 0
            |> List.indexedMap (\n _ -> DropDown (n + 1) Nothing)
    , options =
        Option 0 "-- make a selection --" False
            :: (List.repeat 256 0
                    |> List.indexedMap (\n _ -> Option (n + 1) ("Label for " ++ String.fromInt (n + 1)) False)
               )
    }



-- HELPERS


getOptionFromId : Model -> Int -> Maybe Option
getOptionFromId model id =
    List.filter (\o -> o.id == id) model.options
        |> List.head


disableSelectedOptions : Model -> Model
disableSelectedOptions model =
    let
        -- Get the list of options that have been selected
        selectedOptions : List Option
        selectedOptions =
            List.map (\d -> d.selectedOption) model.dropDowns
                |> List.filterMap identity

        -- Disable the option if it's in the list of selected options
        updateOption : Option -> Option
        updateOption option =
            if List.member option selectedOptions then
                { option | disabled = True }

            else
                option

        -- Disable the only selected options
        updatedOptions =
            List.map (\o -> { o | disabled = False }) model.options
                |> List.map updateOption
    in
    { model | options = updatedOptions }



-- UPDATE


type Msg
    = SelectedItem DropDown String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectedItem changeDropDown value ->
            let
                selectedOption =
                    case String.toInt value of
                        Just id ->
                            getOptionFromId model id

                        Nothing ->
                            Nothing

                updateDropDown selected dropDown =
                    case selected of
                        Just val ->
                            if dropDown.id == changeDropDown.id then
                                { dropDown | selectedOption = Just val }

                            else
                                dropDown

                        Nothing ->
                            if dropDown.id == changeDropDown.id then
                                { dropDown | selectedOption = Nothing }

                            else
                                dropDown

                updatedDropDowns =
                    List.map (updateDropDown selectedOption) model.dropDowns
            in
            { model | dropDowns = updatedDropDowns }
                |> disableSelectedOptions



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "Multiple select elements sharing the same list of embedded options. When an option is selected in any of the dropdowns it will be disabled in the others so it can't be selected again." ]
        , viewDropDowns model
        ]


viewDropDowns : Model -> Html Msg
viewDropDowns model =
    Keyed.node "div"
        [ class "container" ]
        (List.map (viewKeyedDropDown model.options) model.dropDowns)


viewKeyedDropDown : List Option -> DropDown -> ( String, Html Msg )
viewKeyedDropDown options dropdown =
    ( String.fromInt dropdown.id, lazy2 viewDropDown options dropdown )


viewDropDown : List Option -> DropDown -> Html Msg
viewDropDown options dropDown =
    Keyed.node "select"
        [ name (String.fromInt dropDown.id)
        , onInput (SelectedItem dropDown)
        ]
        (List.map viewKeyedOption options)


viewKeyedOption : Option -> ( String, Html Msg )
viewKeyedOption option =
    ( String.fromInt option.id, lazy viewOption option )


viewOption : Option -> Html Msg
viewOption option =
    Html.option [ value (String.fromInt option.id), disabled option.disabled ] [ text option.name ]
