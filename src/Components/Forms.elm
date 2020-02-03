module Components.Forms exposing
    ( FormAttribute(..)
    , FormInputType
    , TextInputType(..)
    , checkboxInput
    , control
    , date
    , email
    , enrollment
    , expandedControl
    , fileInput
    , inputWrapper
    , int
    , password
    , radioInput
    , section
    , selectInput
    , string
    , textInput
    , textareaInput
    , time
    )

import File exposing (File)
import Html exposing (Html, a, br, div, i, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (autocomplete, checked, class, name, placeholder, required, selected, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput)
import Json.Decode as Decode
import List.Extra as List exposing (find)
import Maybe.Extra exposing (filter)
import Models.Info exposing (Enrollment(..), enrollmentFromString, enrollmentToString)
import Utils exposing (Common, isLoadingClass)


type alias FormInputType a =
    { toString : a -> String
    , fromString : String -> a
    , textType : TextInputType
    }


string : FormInputType String
string =
    { toString = identity
    , fromString = identity
    , textType = Text
    }


date : FormInputType String
date =
    { toString = identity
    , fromString = identity
    , textType = Date
    }


time : FormInputType String
time =
    { toString = identity
    , fromString = identity
    , textType = Time
    }


email : FormInputType String
email =
    { toString = identity
    , fromString = identity
    , textType = Email
    }


password : FormInputType String
password =
    { toString = identity
    , fromString = identity
    , textType = Password
    }


int : FormInputType (Maybe Int)
int =
    { toString = Maybe.map String.fromInt >> Maybe.withDefault ""
    , fromString = String.toInt
    , textType = Number
    }


section : Common -> FormInputType (Maybe String)
section common =
    { toString = Maybe.withDefault "No Section"
    , fromString = \s -> common.info.sections |> List.find ((==) s)
    , textType = Text
    }


enrollment : FormInputType (Maybe Enrollment)
enrollment =
    { toString = Maybe.map enrollmentToString >> Maybe.withDefault "Inactive"
    , fromString = enrollmentFromString
    , textType = Text
    }


type FormAttribute
    = Title String
    | Horizontal
    | RequiredField Bool
    | HelpText String
    | Placeholder String
    | IsExpanded
    | IsLoading Bool
    | Prefix String
    | Suffix String
    | Autocomplete


type TextInputType
    = Text
    | Number
    | Email
    | Password
    | Date
    | Time


textTypeToString : TextInputType -> String
textTypeToString type_ =
    case type_ of
        Text ->
            "text"

        Number ->
            "number"

        Email ->
            "email"

        Password ->
            "password"

        Date ->
            "date"

        Time ->
            "time"


control : List (Html msg) -> Html msg
control =
    p [ class "control" ]


expandedControl : List (Html msg) -> Html msg
expandedControl =
    p [ class "control is-expanded" ]


type alias Input a msg =
    { value : a
    , onInput : a -> msg
    , attrs : List FormAttribute
    }


inputWrapper : List FormAttribute -> List (Html msg) -> Html msg
inputWrapper attrs content =
    let
        fieldLabel =
            findTitle attrs
                |> Maybe.map
                    (\title ->
                        if attrs |> List.any ((==) Horizontal) then
                            div [ class "field-label is-normal" ]
                                [ label [ class "label" ] [ text title ] ]

                        else
                            label [ class "label" ] [ text title ]
                    )

        isHorizontal =
            if attrs |> List.any ((==) Horizontal) then
                " is-horizontal"

            else
                ""

        fieldHelpText =
            findHelpText attrs
                |> Maybe.map
                    (\help ->
                        p [ class "help" ] [ text help ]
                    )

        body =
            div [ class "field-body" ]
                content
    in
    div
        [ class <| "field" ++ isHorizontal ]
        ([ fieldLabel, Just body, fieldHelpText ]
            |> List.filterMap identity
        )


fieldWrapperClasses : List FormAttribute -> List String
fieldWrapperClasses attrs =
    let
        isExpanded =
            Just "is-expanded"
                |> filter (\_ -> attrs |> List.member IsExpanded)

        isLoading =
            Just "is-loading"
                |> filter (\_ -> attrs |> List.member (IsLoading True))

        hasAddons =
            Just "has-addons"
                |> filter
                    (\_ ->
                        attrs
                            |> List.any
                                (\attr ->
                                    case attr of
                                        Prefix _ ->
                                            True

                                        Suffix _ ->
                                            True

                                        _ ->
                                            False
                                )
                    )
    in
    [ isLoading, isExpanded, hasAddons ]
        |> List.filterMap identity
        |> (::) "field"


fieldWrapper : List FormAttribute -> List (Html msg) -> Html msg
fieldWrapper attrs content =
    let
        fieldPrefix =
            case prefix attrs of
                Just pre ->
                    [ pre ]

                Nothing ->
                    []

        fieldSuffix =
            case suffix attrs of
                Just suf ->
                    [ suf ]

                Nothing ->
                    []
    in
    div [ class (fieldWrapperClasses attrs |> String.join " ") ]
        (fieldPrefix ++ content ++ fieldSuffix)


findTitle : List FormAttribute -> Maybe String
findTitle attrs =
    attrs
        |> List.filterMap
            (\attr ->
                case attr of
                    Title p ->
                        Just p

                    _ ->
                        Nothing
            )
        |> List.head


findHelpText : List FormAttribute -> Maybe String
findHelpText attrs =
    attrs
        |> List.filterMap
            (\attr ->
                case attr of
                    HelpText p ->
                        Just p

                    _ ->
                        Nothing
            )
        |> List.head


findPlaceholder : List FormAttribute -> Maybe String
findPlaceholder attrs =
    attrs
        |> List.filterMap
            (\attr ->
                case attr of
                    Placeholder p ->
                        Just p

                    _ ->
                        Nothing
            )
        |> List.head


findRequiredField : List FormAttribute -> Maybe Bool
findRequiredField attrs =
    attrs
        |> List.filterMap
            (\attr ->
                case attr of
                    RequiredField r ->
                        Just r

                    _ ->
                        Nothing
            )
        |> List.head


findPrefix : List FormAttribute -> Maybe String
findPrefix attrs =
    attrs
        |> List.filterMap
            (\attr ->
                case attr of
                    Prefix p ->
                        Just p

                    _ ->
                        Nothing
            )
        |> List.head


prefix : List FormAttribute -> Maybe (Html msg)
prefix attrs =
    attrs
        |> findPrefix
        |> Maybe.map
            (\pre ->
                control
                    [ a [ class "button is-static" ]
                        [ text pre ]
                    ]
            )


findSuffix : List FormAttribute -> Maybe String
findSuffix attrs =
    attrs
        |> List.filterMap
            (\attr ->
                case attr of
                    Suffix p ->
                        Just p

                    _ ->
                        Nothing
            )
        |> List.head


suffix : List FormAttribute -> Maybe (Html msg)
suffix attrs =
    attrs
        |> findSuffix
        |> Maybe.map
            (\pre ->
                control
                    [ a [ class "button is-static" ]
                        [ text pre ]
                    ]
            )


textInput : FormInputType a -> Input a msg -> Html msg
textInput inputType data =
    let
        isLoading =
            isLoadingClass
                (data.attrs |> List.member (IsLoading True))

        baseInputAttrs =
            [ class <| "input" ++ isLoading
            , type_ (textTypeToString inputType.textType)
            , value (inputType.toString data.value)
            , onInput (inputType.fromString >> data.onInput)
            ]

        optionalInputAttrs =
            [ findPlaceholder data.attrs
                |> Maybe.map placeholder
            , findRequiredField data.attrs
                |> Maybe.map required
            , data.attrs
                |> find ((==) Autocomplete)
                |> Maybe.map (\_ -> autocomplete True)
            ]

        inputAttrs =
            baseInputAttrs ++ (optionalInputAttrs |> List.filterMap identity)
    in
    inputWrapper data.attrs
        [ fieldWrapper data.attrs
            [ control
                [ input inputAttrs [] ]
            ]
        ]


textareaInput : Input String msg -> Html msg
textareaInput data =
    let
        isLoading =
            isLoadingClass
                (data.attrs |> List.member (IsLoading True))

        baseInputAttrs =
            [ class <| "textarea" ++ isLoading
            , value data.value
            , onInput data.onInput
            ]

        optionalInputAttrs =
            [ findPlaceholder data.attrs
                |> Maybe.map placeholder
            , findRequiredField data.attrs
                |> Maybe.map required
            , data.attrs
                |> find ((==) Autocomplete)
                |> Maybe.map (\_ -> autocomplete True)
            ]

        inputAttrs =
            baseInputAttrs ++ (optionalInputAttrs |> List.filterMap identity)
    in
    inputWrapper data.attrs
        [ fieldWrapper data.attrs
            [ control
                [ textarea inputAttrs [] ]
            ]
        ]


type alias SelectInput a msg =
    { values : List a
    , selected : a
    , onInput : a -> msg
    , attrs : List FormAttribute
    }


selectInput : FormInputType a -> SelectInput a msg -> Html msg
selectInput inputType data =
    let
        isLoading =
            isLoadingClass
                (data.attrs |> List.member (IsLoading True))
    in
    inputWrapper data.attrs
        [ fieldWrapper data.attrs
            [ div
                [ class <| "select control" ++ isLoading ]
                [ select
                    [ onInput (data.onInput << inputType.fromString) ]
                    (data.values
                        |> List.map
                            (\v ->
                                option
                                    [ value <| inputType.toString v
                                    , selected (v == data.selected)
                                    ]
                                    [ text <| inputType.toString v ]
                            )
                    )
                ]
            ]
        ]


radioInput : (a -> String) -> SelectInput a msg -> Html msg
radioInput render data =
    let
        singleOption val =
            label [ class "radio" ]
                [ input
                    [ type_ "radio"
                    , checked <| val == data.selected
                    , onClick <| data.onInput val
                    ]
                    []
                , text <| " " ++ render val
                ]
    in
    inputWrapper data.attrs
        [ fieldWrapper data.attrs
            (data.values
                |> List.map singleOption
                |> List.intersperse (br [] [])
            )
        ]


type alias CheckboxInput msg =
    { content : String
    , isChecked : Bool
    , onChange : Bool -> msg
    }


checkboxInput : CheckboxInput msg -> Html msg
checkboxInput data =
    div
        [ class "control checkbox" ]
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


type alias FileInput msg =
    { file : Maybe File
    , selectFile : Maybe File -> msg
    , attrs : List FormAttribute
    }


filesDecoder : Decode.Decoder (Maybe File)
filesDecoder =
    Decode.at [ "target", "files" ]
        (Decode.list File.decoder |> Decode.map List.head)


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
    inputWrapper data.attrs <|
        [ div [ class "file has-name" ]
            [ label [ class "file-label" ]
                [ inputElement
                , uploadButton
                , maybeFileName
                ]
            ]
        ]
