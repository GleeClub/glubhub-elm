module Components.DeleteModal exposing (deleteModal)

import Components.Basics as Basics
import Html exposing (Html, button, div, footer, header, p, section, text)
import Html.Attributes exposing (attribute, class)
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
            [ data.title |> modalHeader data.cancel
            , modalBody data.content
            , modalButtons data
            ]
        ]


modalHeader : msg -> String -> Html msg
modalHeader cancel title =
    header [ class "modal-card-head" ]
        [ p [ class "modal-card-title" ]
            [ text title ]
        , button
            [ class "delete"
            , attribute "aria-label" "close"
            , onClick cancel
            ]
            []
        ]


modalBody : Html msg -> Html msg
modalBody content =
    section [ class "modal-card-body" ]
        [ content ]


modalButtons : DeleteModal msg -> Html msg
modalButtons data =
    footer [ class "modal-card-foot" ]
        [ button
            [ class <| "button is-danger" ++ Utils.isLoadingClass (data.state == Sending)
            , onClick data.confirm
            ]
            [ text "Delete" ]
        , button
            [ class "button"
            , onClick data.cancel
            ]
            [ text "Cancel" ]
        , case data.state of
            ErrorSending error ->
                Basics.errorBox error

            _ ->
                text ""
        ]
