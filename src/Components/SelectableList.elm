module Components.SelectableList exposing (SelectableListData, selectableList)

import Html exposing (Html, a, button, div, form, h1, img, input, label, p, section, span, table, tbody, td, text, tfoot, thead, tr)
import Html.Attributes exposing (class, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick)
import Maybe.Extra exposing (filter, isJust)
import Utils exposing (RemoteData(..), spinner)


type alias SelectableListData a msg =
    { listItems : RemoteData (List a)
    , render : a -> List (Html msg)
    , isSelected : a -> Bool
    , onSelect : a -> msg
    , messageIfEmpty : String
    }


selectableList : SelectableListData a msg -> Html msg
selectableList data =
    let
        render listItem =
            tr
                [ style "background-color" <|
                    if data.isSelected listItem then
                        "#eeeeee"

                    else
                        ""
                , onClick <| data.onSelect listItem
                ]
                (data.render listItem)

        content =
            case data.listItems of
                NotAsked ->
                    text ""

                Loading ->
                    spinner

                Loaded [] ->
                    p [] [ text data.messageIfEmpty ]

                Loaded elements ->
                    div []
                        [ table [ class "table is-fullwidth is-hoverable" ]
                            [ thead [] []
                            , tbody [] <| List.map render elements
                            , tfoot [] []
                            ]
                        ]

                Failure ->
                    p [] [ text "Sorry, I broke something :(" ]
    in
    div [ class "column is-narrow" ]
        [ div
            [ class "box" ]
            [ content ]
        ]
