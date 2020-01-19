module Page.NotFound exposing (notFound)

import Components.Basics as Basics
import Html exposing (Html, a, div, p, text)
import Html.Attributes exposing (class, href)
import Utils exposing (Common)


notFound : Common -> Html msg
notFound _ =
    div [ class "container fullheight" ]
        [ div [ class "columns is-centered is-vcentered" ]
            [ Basics.narrowColumn
                [ Basics.title "(404) not-found"
                , p []
                    [ a
                        [ href "mailto:sam@mohr.codes" ]
                        [ text "Personally text Sam Mohr" ]
                    , text " if you think there's an error."
                    ]
                ]
            ]
        ]
