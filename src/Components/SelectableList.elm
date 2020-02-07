module Components.SelectableList exposing (selectableList, selectableListFull, selectableListWithDividers)

import Color
import Components.Basics as Basics exposing (remoteContent)
import Html exposing (Html, div, p, table, tbody, text, tfoot, thead, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Utils exposing (RemoteData(..), goldColor, mapLoaded)


type alias SelectableListFunctions a msg b =
    { b
        | render : a -> List (Html msg)
        , isSelected : a -> Bool
        , onSelect : a -> msg
    }


type alias SelectableList a msg b =
    SelectableListFunctions a
        msg
        { b
            | listItems : RemoteData (List a)
            , messageIfEmpty : String
        }


type alias SelectableListWithDividers a msg b =
    SelectableListFunctions a
        msg
        { b
            | listItems : RemoteData (List (List a))
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
    Basics.narrowColumn
        [ Basics.box
            [ data.listItems |> remoteContent (allRows data) ]
        ]


selectableListFull : SelectableListFull a msg -> Html msg
selectableListFull data =
    Basics.narrowColumn
        [ Basics.box
            [ data.contentAtTop
            , data.listItems |> remoteContent (allRows data)
            , data.contentAtBottom
            ]
        ]


selectableListWithDividers : SelectableListWithDividers a msg b -> Html msg
selectableListWithDividers data =
    Basics.narrowColumn
        [ Basics.box
            [ data.listItems
                |> mapLoaded (List.filter (List.isEmpty >> not))
                |> remoteContent (allRowsWithDividers data)
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


allRowsWithDividers : SelectableListWithDividers a msg b -> List (List a) -> Html msg
allRowsWithDividers data rowGroups =
    if List.isEmpty rowGroups then
        p [] [ text data.messageIfEmpty ]

    else
        div []
            [ table [ class "table is-fullwidth is-hoverable no-bottom-border" ]
                [ thead [] []
                , tbody []
                    (rowGroups
                        |> List.map (List.map (singleRow data))
                        |> List.intersperse [ dividerRow ]
                        |> List.concat
                    )
                , tfoot [] []
                ]
            ]


dividerRow : Html msg
dividerRow =
    tr [ class "not-hoverable" ]
        [ div [ class "is-divider", style "margin" "1rem" ] [] ]


singleRow : SelectableListFunctions a msg b -> a -> Html msg
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
