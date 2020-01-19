module Page.Repertoire.SongSidebar exposing (songSidebar)

import Components.Basics as Basics
import Html exposing (Html, b, br, button, div, p, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Models.Permissions as Permissions
import Models.Song exposing (Pitch, Song, SongLinkSection, SongMode, pitchToString, songModeToString)
import Page.Repertoire.Links exposing (songLinkButton)
import Utils exposing (Common, RemoteData)


type alias SongSidebar msg =
    { common : Common
    , song : RemoteData Song
    , close : msg
    , edit : msg
    , playPitch : Pitch -> msg
    }


songSidebar : SongSidebar msg -> Html msg
songSidebar data =
    Basics.sidebar
        { data = data.song
        , render = viewSelectedSong data
        , close = data.close
        }


viewSelectedSong : SongSidebar msg -> Song -> Html msg
viewSelectedSong data song =
    div []
        [ Basics.backTextButton "all songs" data.close
        , Basics.title song.title
        , song.info
            |> Maybe.map (\info -> p [] [ text info, br [] [] ])
            |> Maybe.withDefault (text "")
        , pitchSection data.playPitch "Key" song.mode song.key
        , pitchSection data.playPitch "Starting pitch" Nothing song.startingPitch
        , br [] []
        , linkTable song.links
        , Basics.renderIfHasPermission data.common Permissions.editRepertoire <|
            button [ class "button", onClick data.edit ]
                [ text "Edit Song" ]
        ]


pitchSection : (Pitch -> msg) -> String -> Maybe SongMode -> Maybe Pitch -> Html msg
pitchSection playPitch name maybeMode maybePitch =
    let
        modeText =
            maybeMode
                |> Maybe.map (\m -> " " ++ songModeToString m)
                |> Maybe.withDefault ""

        pitchText =
            case maybePitch of
                Just pitch ->
                    b
                        ((onClick <| playPitch pitch)
                            :: Basics.tooltip "Hey kid, wanna pitch?"
                        )
                        [ text <| pitchToString pitch ++ modeText ]

                Nothing ->
                    b [] [ text "?" ]
    in
    p []
        [ text <| name ++ ": "
        , pitchText
        ]


linkTable : List SongLinkSection -> Html msg
linkTable linkSections =
    table [ class "table is-fullwidth" ] <| List.filterMap linkSection linkSections


linkSection : SongLinkSection -> Maybe (Html msg)
linkSection songLinkSection =
    if List.isEmpty songLinkSection.links then
        Nothing

    else
        Just <|
            tr [ style "border" "none" ]
                [ td [ style "border" "none" ]
                    [ text songLinkSection.name ]
                , td [ style "border" "none" ]
                    (songLinkSection.links
                        |> List.map songLinkButton
                        |> List.intersperse (text " ")
                    )
                ]
