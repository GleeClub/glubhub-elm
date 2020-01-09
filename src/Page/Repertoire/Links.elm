module Page.Repertoire.Links exposing (songLinkButton, songLinkButtonWithDelete)

import Html exposing (Html, a, button, i, p, span, text)
import Html.Attributes exposing (class, href, style, target)
import Html.Events exposing (onClick)
import List.Extra as List
import Models.Song exposing (SongLink)


songLinkButtonWithDelete : (Int -> msg) -> SongLink -> Html msg
songLinkButtonWithDelete deleteMsg link =
    span
        []
        [ songLinkButton link
        , button [ class "delete", onClick (deleteMsg link.id) ] []
        ]


songLinkButton : SongLink -> Html msg
songLinkButton link =
    let
        knownRenderers =
            [ ( "Sheet Music", "https://gleeclub.gatech.edu/music/", sheetMusicLink )
            , ( "MIDIs", "https://gleeclub.gatech.edu/music/", midiLink )
            , ( "Performances", "https://youtu.be/", videoLink )
            ]

        defaultButton songLink =
            span [ class "button", href songLink.target ] [ text songLink.name ]
    in
    knownRenderers
        |> List.find (\( type_, _, _ ) -> type_ == link.type_)
        |> Maybe.map (\( _, urlBase, renderer ) -> renderer (urlBase ++ link.target) link.name)
        |> Maybe.withDefault (defaultButton link)


sheetMusicLink : String -> String -> Html msg
sheetMusicLink url name =
    a
        [ class "button is-outlined is-primary"
        , href url
        ]
        [ span [ class "icon" ]
            [ i [ class "fas fa-scroll" ] [] ]
        , span [] [ text name ]
        ]


midiLink : String -> String -> Html msg
midiLink url name =
    a
        [ class "button is-outlined is-primary"
        , href url
        ]
        [ span [ class "icon" ]
            [ i [ class "fas fa-volume-up" ] [] ]
        , span [] [ text name ]
        ]


videoLink : String -> String -> Html msg
videoLink url name =
    span
        [ style "display" "flex"
        , style "align-items" "center"
        ]
        [ span
            [ class "icon has-text-grey-lighter"
            , style "margin-right" ".5rem"
            ]
            [ i [ class "fas fa-external-link-alt" ] [] ]
        , a
            [ class "button"
            , href url
            , target "_blank"
            ]
            [ span [ class "icon has-text-danger" ]
                [ i [ class "fab fa-youtube" ] [] ]
            ]
        , p []
            [ span [ style "padding-left" "5px" ]
                [ text name ]
            ]
        ]
