module Components.Basics exposing (HorizontalField, box, checkOrCross, column, columns, horizontalField, linkButton, narrowColumn, title, tooltip)

import Html exposing (Html, a, button, div, form, h1, i, img, input, label, p, section, span, table, tbody, td, text, tfoot, thead, tr)
import Html.Attributes exposing (attribute, class, href, id, name, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Route exposing (Route)


title : String -> Html msg
title content =
    h1 [ class "title", style "text-align" "center" ] [ text content ]


column : List (Html msg) -> Html msg
column content =
    div [ class "column" ] content


columns : List (Html msg) -> Html msg
columns content =
    div [ class "columns" ] content


narrowColumn : List (Html msg) -> Html msg
narrowColumn content =
    div [ class "column is-narrow" ] content


box : List (Html msg) -> Html msg
box content =
    div [ class "box" ] content


tooltip : String -> Html.Attribute msg
tooltip content =
    attribute "data-tooltip" content


checkOrCross : Bool -> Html msg
checkOrCross isCheck =
    span [ class "icon is-medium" ]
        [ i
            [ class <|
                "fas fa-lg "
                    ++ (if isCheck then
                            "fa-check"

                        else
                            "fa-times"
                       )
            ]
            []
        ]


linkButton : String -> Route -> Html msg
linkButton content route =
    a [ class "button", Route.href route ] [ text content ]


type alias HorizontalField msg =
    { label : String
    , name : String
    , type_ : String
    , value : String
    , placeholder : String
    , onInput : String -> msg
    }


horizontalField : HorizontalField msg -> Html msg
horizontalField field =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label", attribute "for" field.name ] [ text field.label ] ]
        , div [ class "control" ]
            [ input
                [ class "input"
                , name field.name
                , type_ field.type_
                , value field.value
                , onInput field.onInput
                , placeholder field.placeholder
                ]
                []
            ]
        ]
