module Components.SelectableList exposing (selectableList)

import Color exposing (Color)
import Components.Basics exposing (remoteContent)
import Html exposing (Html, a, button, div, form, h1, img, input, label, p, section, span, table, tbody, td, text, tfoot, thead, tr)
import Html.Attributes exposing (class, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick)
import Maybe.Extra exposing (filter, isJust)
import Utils exposing (RemoteData(..), goldColor)


type alias SelectableListData a msg =
    { listItems : RemoteData (List a)
    , render : a -> List (Html msg)
    , isSelected : a -> Bool
    , onSelect : a -> msg
    , messageIfEmpty : String
    }


selectableList : SelectableListData a msg -> Html msg
selectableList data =
    div [ class "column is-narrow" ]
        [ div [ class "box" ]
            [ data.listItems |> remoteContent (allRows data) ]
        ]


allRows : SelectableListData a msg -> List a -> Html msg
allRows data items =
    if List.isEmpty items then
        p [] [ text data.messageIfEmpty ]

    else
        div []
            [ table [ class "table is-fullwidth is-hoverable" ]
                [ thead [] []
                , tbody [] (items |> List.map (singleRow data))
                , tfoot [] []
                ]
            ]


singleRow : SelectableListData a msg -> a -> Html msg
singleRow data listItem =
    tr
        [ style "background-color" <|
            if data.isSelected listItem then
                Color.toCssString goldColor

            else
                ""
        , onClick <| data.onSelect listItem
        ]
        (data.render listItem)
