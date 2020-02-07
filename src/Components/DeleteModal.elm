module Components.DeleteModal exposing (deleteModal)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Html exposing (Html, button, div, footer, header, p, section, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Utils exposing (SubmissionState(..))


type alias DeleteModal msg =
    { title : String
    , content : Html msg
    , cancel : msg
    , confirm : msg
    , state : SubmissionState
    }


deleteModal : DeleteModal msg -> Html msg
deleteModal data =
    div [ class "modal is-active" ]
        [ div [ class "modal-background", onClick data.cancel ] []
        , div [ class "modal-card" ]
            [ modalHeader data.cancel data.title
            , modalBody data.content
            , modalButtons data
            ]
        ]


modalHeader : msg -> String -> Html msg
modalHeader cancel title =
    header [ class "modal-card-head" ]
        [ p [ class "modal-card-title" ]
            [ text title ]
        , Buttons.delete cancel
        ]


modalBody : Html msg -> Html msg
modalBody content =
    section [ class "modal-card-body" ]
        [ content ]


modalButtons : DeleteModal msg -> Html msg
modalButtons data =
    footer [ class "modal-card-foot" ]
        [ Buttons.button
            { content = "Delete"
            , onClick = Just data.confirm
            , attrs =
                [ Buttons.IsLoading (data.state == Sending)
                , Buttons.Color Buttons.IsDanger
                ]
            }
        , Buttons.button
            { content = "Cancel"
            , onClick = Just data.cancel
            , attrs = []
            }
        , case data.state of
            ErrorSending error ->
                Basics.errorBox error

            _ ->
                text ""
        ]
