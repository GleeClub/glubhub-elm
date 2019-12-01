module Page.Events.RequestAbsence exposing (requestAbsence)

import Components.Basics as Basics
import Html exposing (Html, a, br, button, div, form, h1, h2, img, label, section, table, tbody, td, text, textarea, thead, tr)
import Html.Attributes exposing (class, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Encode as Encode
import Maybe.Extra
import Models.Event exposing (FullEvent)
import Route exposing (EventTab, Route)
import Time exposing (Posix)
import Utils exposing (Common, alert, apiUrl, postRequest)


type alias RequestAbsence msg =
    { reason : String
    , event : FullEvent
    , updateReason : String -> msg
    , submit : msg
    , cancel : msg
    }


requestAbsence : RequestAbsence msg -> Html msg
requestAbsence data =
    div []
        [ Basics.backTextButton "back to event" data.cancel
        , h1 [ class "title", style "text-align" "center" ] [ text "Absence Request" ]
        , h2 [ class "subtitle", style "text-align" "center" ] [ text <| "for " ++ data.event.name ]
        , br [] []
        , form [ id "absence-request", onSubmit data.submit ]
            [ div [ class "field" ]
                [ label [ class "label" ] [ text "But y tho" ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , value data.reason
                        , onInput data.updateReason
                        , placeholder "Excuses, excuses"
                        ]
                        []
                    ]
                ]
            , button [ type_ "submit", class "button is-primary is-right" ]
                [ text "Beg for Mercy" ]
            ]
        ]
