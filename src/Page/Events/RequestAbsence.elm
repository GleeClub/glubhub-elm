module Page.Events.RequestAbsence exposing (requestAbsence)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.Forms as Forms exposing (textareaInput)
import Html exposing (Html, br, div)
import Html.Attributes exposing (style)
import Models.Event exposing (Event)


type alias RequestAbsence msg =
    { reason : String
    , event : Event
    , updateReason : String -> msg
    , submit : msg
    , cancel : msg
    }


requestAbsence : RequestAbsence msg -> Html msg
requestAbsence data =
    div [ style "text-align" "center" ]
        [ Buttons.back
            { content = "back to event"
            , onClick = data.cancel
            }
        , Basics.title "Absence Request"
        , Basics.subtitle <| "for " ++ data.event.name
        , br [] []
        , Basics.form data.submit
            [ textareaInput
                { value = data.reason
                , onInput = data.updateReason
                , attrs =
                    [ Forms.Title "But y tho"
                    , Forms.Placeholder "Excuses, excuses"
                    , Forms.RequiredField True
                    ]
                }
            , Buttons.group
                { alignment = Buttons.AlignRight
                , connected = False
                , buttons =
                    [ Buttons.submit
                        { content = "Beg for Mercy"
                        , attrs = [ Buttons.Color Buttons.IsPrimary ]
                        }
                    ]
                }
            ]
        ]
