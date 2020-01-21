module Components.Forms exposing
    ( checkboxInput
    , dateInput
    , emailInput
    , fieldWrapper
    , fileInput
    , genericTextInput
    , numberInput
    , numberInputWithPrefix
    , passwordInput
    , selectInput
    , textInput
    , textInputWithPrefix
    , textareaInput
    , timeInput
    )

import File exposing (File)
import Html exposing (Html, a, div, i, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (checked, class, name, placeholder, required, selected, type_, value)
import Html.Events exposing (on, onCheck, onInput)
import Json.Decode as Decode


type alias FieldInput a =
    { a
        | title : String
        , helpText : Maybe String
    }


fieldWrapper : FieldInput a -> List (Html msg) -> Html msg
fieldWrapper field content =
    div [ class "field" ]
        [ label [ class "label" ] [ text field.title ]
        , div [ class "control" ] content
        , field.helpText
            |> Maybe.map (\help -> p [ class "help" ] [ text help ])
            |> Maybe.withDefault (text "")
        ]


type alias TextInput msg =
    { title : String
    , helpText : Maybe String
    , value : String
    , placeholder : String
    , required : Bool
    , onInput : String -> msg
    }


genericTextInput : String -> TextInput msg -> Html msg
genericTextInput inputType data =
    fieldWrapper data
        [ input
            [ class "input"
            , type_ inputType
            , placeholder data.placeholder
            , value data.value
            , required data.required
            , onInput data.onInput
            ]
            []
        ]


textInput : TextInput msg -> Html msg
textInput =
    genericTextInput "text"


numberInput : TextInput msg -> Html msg
numberInput =
    genericTextInput "number"


dateInput : TextInput msg -> Html msg
dateInput =
    genericTextInput "date"


timeInput : TextInput msg -> Html msg
timeInput =
    genericTextInput "time"


emailInput : TextInput msg -> Html msg
emailInput =
    genericTextInput "email"


passwordInput : TextInput msg -> Html msg
passwordInput =
    genericTextInput "password"


type alias TextInputWithPrefix msg =
    { title : String
    , prefix : String
    , helpText : Maybe String
    , value : String
    , placeholder : String
    , required : Bool
    , onInput : String -> msg
    }


textInputWithPrefix : TextInputWithPrefix msg -> Html msg
textInputWithPrefix data =
    fieldWrapper data <|
        [ div [ class "field has-addons" ]
            [ p [ class "control" ]
                [ a [ class "button is-static" ]
                    [ text data.prefix ]
                ]
            , p [ class "control" ]
                [ input
                    [ class "input"
                    , type_ "text"
                    , placeholder data.placeholder
                    , value data.value
                    , required data.required
                    , onInput data.onInput
                    ]
                    []
                ]
            ]
        ]


numberInputWithPrefix : TextInputWithPrefix msg -> Html msg
numberInputWithPrefix data =
    fieldWrapper data <|
        [ div [ class "field has-addons" ]
            [ p [ class "control" ]
                [ a [ class "button is-static" ]
                    [ text data.prefix ]
                ]
            , p [ class "control" ]
                [ input
                    [ class "input"
                    , type_ "number"
                    , placeholder data.placeholder
                    , value data.value
                    , required data.required
                    , onInput data.onInput
                    ]
                    []
                ]
            ]
        ]


type alias CheckboxInput msg =
    { content : String
    , isChecked : Bool
    , onChange : Bool -> msg
    }


checkboxInput : CheckboxInput msg -> Html msg
checkboxInput data =
    div [ class "field" ]
        [ div [ class "control" ]
            [ label [ class "checkbox" ]
                [ input
                    [ type_ "checkbox"
                    , checked data.isChecked
                    , onCheck data.onChange
                    ]
                    []
                , text <| " " ++ data.content
                ]
            ]
        ]


textareaInput : TextInput msg -> Html msg
textareaInput data =
    fieldWrapper data
        [ textarea
            [ class "textarea"
            , required data.required
            , placeholder data.placeholder
            , value data.value
            , onInput data.onInput
            ]
            []
        ]


type alias SelectInput a msg =
    { title : String
    , helpText : Maybe String
    , values : List a
    , render : a -> ( String, String )
    , loading : Bool
    , selected : a -> Bool
    , onSelect : String -> msg
    }


selectInput : SelectInput a msg -> Html msg
selectInput data =
    div [ class "field" ]
        [ label [ class "label" ] [ text data.title ]
        , div
            [ class <|
                "select control"
                    ++ (if data.loading then
                            " is-loading"

                        else
                            ""
                       )
            ]
            [ select
                [ onInput data.onSelect ]
                (data.values
                    |> List.map
                        (\v ->
                            let
                                ( optionVal, textVal ) =
                                    data.render v
                            in
                            option
                                [ value optionVal
                                , selected (data.selected v)
                                ]
                                [ text textVal ]
                        )
                )
            ]
        , data.helpText
            |> Maybe.map (\help -> p [ class "help" ] [ text help ])
            |> Maybe.withDefault (text "")
        ]


type alias FileInput msg =
    { title : String
    , helpText : Maybe String
    , file : Maybe File
    , selectFile : List File -> msg
    }


filesDecoder : Decode.Decoder (List File)
filesDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)


fileInput : FileInput msg -> Html msg
fileInput data =
    let
        uploadButton =
            span [ class "file-cta" ]
                [ span [ class "file-icon" ]
                    [ i [ class "fas fa-upload" ] [] ]
                , span [ class "file-label" ]
                    [ text "Choose a file..." ]
                ]

        inputElement =
            input
                [ class "file-input"
                , type_ "file"
                , on "change" (Decode.map data.selectFile filesDecoder)
                ]
                []

        maybeFileName =
            data.file
                |> Maybe.map
                    (\file ->
                        span
                            [ class "file-name" ]
                            [ text (File.name file) ]
                    )
                |> Maybe.withDefault (text "")
    in
    fieldWrapper data <|
        [ div [ class "file has-name" ]
            [ label [ class "file-label" ]
                [ inputElement
                , uploadButton
                , maybeFileName
                ]
            ]
        ]
