module Components.SelectableList exposing (selectableList, selectableListFull)

import Color
import Components.Basics exposing (remoteContent)
import Html exposing (Html, div, p, table, tbody, text, tfoot, thead, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Utils exposing (RemoteData(..), goldColor)


type alias SelectableList a msg b =
    { b
        | listItems : RemoteData (List a)
        , render : a -> List (Html msg)
        , isSelected : a -> Bool
        , onSelect : a -> msg
        , messageIfEmpty : String
    }


type alias SelectableListFull a msg =
    SelectableList a
        msg
        { contentAtTop : Html msg
        , contentAtBottom : Html msg
        }


selectableList : SelectableList a msg b -> Html msg
selectableList data =
    div [ class "column is-narrow" ]
        [ div [ class "box" ]
            [ data.listItems |> remoteContent (allRows data) ]
        ]


selectableListFull : SelectableListFull a msg -> Html msg
selectableListFull data =
    div [ class "column is-narrow" ]
        [ div [ class "box" ]
            [ data.contentAtTop
            , data.listItems |> remoteContent (allRows data)
            , data.contentAtBottom
            ]
        ]


allRows : SelectableList a msg b -> List a -> Html msg
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


singleRow : SelectableList a msg b -> a -> Html msg
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
