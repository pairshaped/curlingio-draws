module Selects exposing (..)

import Browser
import Data exposing (..)
import Html exposing (Html, div, option, p, select, text)
import Html.Attributes exposing (disabled, name, selected, style, value)
import Html.Events exposing (onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)


main =
    Browser.sandbox { init = init, update = update, view = view }


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


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "Multiple select elements sharing the same list of embedded options. When an option is selected in any of the dropdowns it will be disabled in the others so it can't be selected again." ]
        , viewDropDowns model
        ]


viewOption : Option -> Html Msg
viewOption option =
    Html.option [ value (String.fromInt option.id), disabled option.disabled ] [ text option.name ]


viewDropDowns : Model -> Html Msg
viewDropDowns model =
    div [ style "display" "flex", style "flex-wrap" "wrap" ]
        (List.map (viewDropDown model.options) model.dropDowns)


viewDropDown : List Option -> DropDown -> Html Msg
viewDropDown options dropDown =
    select
        [ style "padding" "5px"
        , style "margin" "5px"
        , name (String.fromInt dropDown.id)
        , onInput (SelectedItem dropDown)
        ]
        (Html.option [ disabled True, selected True ] [ text "-- make a selection --" ]
            :: List.map (lazy viewOption) options
        )
