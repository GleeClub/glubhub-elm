module Page.Repertoire.SongSidebar exposing (songSidebar)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Html exposing (Html, b, br, div, p, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Models.Song exposing (Pitch, Song, SongLinkSection, SongMode, pitchToUnicode, songModeToString)
import Page.Repertoire.Links exposing (songLinkButton)
import Permissions
import Utils exposing (Common, RemoteData)


type alias SongSidebar msg =
    { common : Common
    , song : RemoteData Song
    , close : msg
    , edit : msg
    , tryToDelete : msg
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
        [ Buttons.back
            { content = "all songs"
            , onClick = data.close
            }
        , Basics.centeredTitle song.title
        , song.info
            |> Maybe.map (\info -> p [] [ text info, br [] [] ])
            |> Maybe.withDefault (text "")
        , pitchSection data.playPitch "Key" song.mode song.key
        , pitchSection data.playPitch "Starting pitch" Nothing song.startingPitch
        , br [] []
        , linkTable song.links
        , Basics.renderIfHasPermission data.common Permissions.editRepertoire <|
            div []
                [ Buttons.button
                    { content = "Edit Song"
                    , onClick = Just data.edit
                    , attrs = []
                    }
                , br [] []
                , br [] []
                , Buttons.button
                    { content = "Delete Song"
                    , onClick = Just data.tryToDelete
                    , attrs = [ Buttons.Color Buttons.IsDanger ]
                    }
                ]
        ]


pitchSection : (Pitch -> msg) -> String -> Maybe SongMode -> Maybe Pitch -> Html msg
pitchSection playPitch sectionName maybeMode maybePitch =
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
                        [ text <| pitchToUnicode pitch ++ modeText ]

                Nothing ->
                    b [] [ text "?" ]
    in
    p []
        [ text <| sectionName ++ ": "
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
