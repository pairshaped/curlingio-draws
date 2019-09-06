module DataList exposing (..)

import Browser
import Data exposing (..)
import Html exposing (Html, datalist, div, input, p, text)
import Html.Attributes exposing (disabled, id, list, name, style, value)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


getOptionFromName : Model -> String -> Maybe Option
getOptionFromName model name =
    List.filter (\o -> o.name == name) model.options
        |> List.head



-- When an option is selected, disable the option that was selected.


type Msg
    = SelectedItem DropDown String


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectedItem changeDropDown name ->
            let
                selectedOption =
                    getOptionFromName model name

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
        [ p [] [ text "Used the datalist element to repeat a list of dropdowns that share the same options. When an option is selected in any of the dropdowns it will be disabled in the datalist and can't be selected again." ]
        , datalist [ id "options" ] (List.map viewOption model.options)
        , viewDropDowns model.dropDowns
        ]


viewOption : Option -> Html Msg
viewOption option =
    Html.option [ disabled option.disabled ] [ text option.name ]


viewDropDowns : List DropDown -> Html Msg
viewDropDowns dropDowns =
    div [ style "display" "flex", style "flex-wrap" "wrap" ] (List.map viewDropDown dropDowns)


viewDropDown : DropDown -> Html Msg
viewDropDown dropDown =
    input
        [ style "padding" "5px"
        , style "margin" "5px"
        , list "options"
        , name (String.fromInt dropDown.id)
        , onInput (SelectedItem dropDown)
        ]
        []
