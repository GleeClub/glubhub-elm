module Page.Repertoire exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Components.SelectableList exposing (selectableList)
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, form, h1, i, img, input, label, p, section, span, table, tbody, td, text, tr)
import Html.Attributes exposing (class, href, id, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (find)
import MD5
import Maybe.Extra exposing (filter, isJust)
import Models.Event exposing (Member, memberDecoder)
import Models.Song exposing (Pitch, Song, SongLink, SongLinkSection, SongMode, halfStepsAboveA, pitchToString, songDecoder, songModeToString)
import Route exposing (Route)
import Task
import Utils exposing (Common, RemoteData(..), getRequest, mapLoaded, playPitch, remoteToMaybe, resultToRemote, scrollToElement)



---- MODEL ----


type alias Model =
    { common : Common
    , songs : RemoteData (List Song)
    , selected : RemoteData Song
    }


init : Common -> Maybe Int -> ( Model, Cmd Msg )
init common maybeSongId =
    let
        ( toLoadSong, selected ) =
            case maybeSongId of
                Just songId ->
                    ( [ loadSong common songId ], Loading )

                Nothing ->
                    ( [], NotAsked )
    in
    ( { common = common, songs = Loading, selected = selected }
    , Cmd.batch <| loadSongs common :: toLoadSong
    )


songDivId : String
songDivId =
    "musicDeetsBox"



---- UPDATE ----


type Msg
    = OnLoadSongs (GreaseResult (List Song))
    | OnLoadSong (GreaseResult Song)
    | SelectSong Int
      -- | OpenEditSong
    | UnselectSong
    | PlayPitch Pitch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadSongs songsResult ->
            ( { model | songs = resultToRemote songsResult }, Cmd.none )

        OnLoadSong (Ok song) ->
            ( { model | selected = Loaded song }, scrollToElement songDivId )

        OnLoadSong (Err error) ->
            ( { model | selected = Failure error }, Cmd.none )

        SelectSong selected ->
            ( { model | selected = Loading }
            , Cmd.batch
                [ loadSong model.common selected
                , Route.replaceUrl model.common.key <| Route.Repertoire (Just selected)
                ]
            )

        UnselectSong ->
            ( { model | selected = NotAsked }, Cmd.none )

        PlayPitch pitch ->
            ( model, playPitch (halfStepsAboveA pitch) )



---- DATA ----


loadSongs : Common -> Cmd Msg
loadSongs common =
    getRequest common "/repertoire" (Decode.list songDecoder)
        |> Task.attempt OnLoadSongs


loadSong : Common -> Int -> Cmd Msg
loadSong common songId =
    let
        url =
            "/repertoire/" ++ String.fromInt songId ++ "?details=true"
    in
    getRequest common url songDecoder
        |> Task.attempt OnLoadSong


getLink : String -> String -> String
getLink type_ target =
    case type_ of
        "pdf" ->
            "/music/" ++ target

        "midi" ->
            "/music/" ++ target

        "performance" ->
            "https://youtu.be/" ++ target

        _ ->
            target



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        currentSongsFilter =
            .current

        otherAToGFilter song =
            not song.current && (song.title |> String.left 1 |> String.toLower) < "h"

        otherHToPFilter song =
            let
                firstChar =
                    song.title |> String.left 1 |> String.toLower
            in
            not song.current && firstChar > "g" && firstChar < "q"

        otherQToZFilter song =
            not song.current && (song.title |> String.left 1 |> String.toLower) > "p"
    in
    div [ id "repertoire" ]
        [ section
            [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ songList model "Current" currentSongsFilter
                    , songList model "A-G" otherAToGFilter
                    , songList model "H-P" otherHToPFilter
                    , songList model "Q-Z" otherQToZFilter
                    ]
                ]
            ]
        , songSidebar model.selected
        ]


songList : Model -> String -> (Song -> Bool) -> Html Msg
songList model title filter =
    div [ class "column is-one-quarter is-centered" ]
        [ Basics.title title
        , selectableList
            { listItems = model.songs |> mapLoaded (List.filter filter)
            , render = \song -> [ td [] [ text song.title ] ]
            , onSelect = \song -> SelectSong song.id
            , messageIfEmpty = "Nothin' to see here, buddy."
            , isSelected =
                \song ->
                    model.selected
                        |> remoteToMaybe
                        |> Maybe.map (\s -> s.id == song.id)
                        |> Maybe.withDefault False
            }
        ]


songSidebar : RemoteData Song -> Html Msg
songSidebar remoteSong =
    Basics.sidebar
        { data = remoteSong
        , render = viewSelectedSong
        , close = UnselectSong
        }


viewSelectedSong : Song -> Html Msg
viewSelectedSong song =
    div []
        [ Basics.title song.title
        , song.info
            |> Maybe.map (\info -> p [] [ text info, br [] [] ])
            |> Maybe.withDefault (text "")
        , pitchSection "Key" song.mode song.key
        , pitchSection "Starting pitch" Nothing song.startingPitch
        , br [] []
        , linkTable (song.links |> Maybe.withDefault [])
        ]


pitchSection : String -> Maybe SongMode -> Maybe Pitch -> Html Msg
pitchSection name maybeMode maybePitch =
    let
        pitchText =
            case maybePitch of
                Just pitch ->
                    b
                        ((onClick <| PlayPitch pitch)
                            :: Basics.tooltip "Hey kid, wanna pitch?"
                        )
                        [ text <| pitchToString pitch ]

                Nothing ->
                    b [] [ text "?" ]
    in
    p []
        [ text <| name ++ ": "
        , pitchText
        , text (maybeMode |> Maybe.map (\m -> " " ++ songModeToString m) |> Maybe.withDefault "")
        ]


linkTable : List SongLinkSection -> Html Msg
linkTable linkSections =
    table [ class "table is-fullwidth" ] <| List.map linkSection linkSections


linkSection : SongLinkSection -> Html Msg
linkSection songLinkSection =
    let
        viewSection =
            case ( songLinkSection.name, List.isEmpty songLinkSection.links ) of
                ( "Sheet Music", False ) ->
                    Just sheetMusicLink

                ( "MIDIs", False ) ->
                    Just midiLink

                ( "Performances", False ) ->
                    Just videoLink

                _ ->
                    Nothing
    in
    case viewSection of
        Just viewer ->
            tr [ style "border" "none" ]
                [ td [ style "border" "none" ] [ text songLinkSection.name ]
                , td [ style "border" "none" ] <|
                    (List.map viewer songLinkSection.links |> List.intersperse (text " "))
                ]

        Nothing ->
            tr [ style "display" "none" ] []


sheetMusicLink : SongLink -> Html Msg
sheetMusicLink link =
    a
        [ class "button is-outlined is-primary"
        , href <| getLink "pdf" link.target
        ]
        [ span [ class "icon" ]
            [ i [ class "fas fa-scroll" ] [] ]
        , span [] [ text link.name ]
        ]


midiLink : SongLink -> Html Msg
midiLink link =
    a
        [ class "button is-outlined is-primary"
        , href <| getLink "midi" link.target
        ]
        [ span [ class "icon" ]
            [ i [ class "fas fa-volume-up" ] [] ]
        , span [] [ text link.name ]
        ]


videoLink : SongLink -> Html Msg
videoLink link =
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
            , href <| getLink "performance" link.target
            , target "_blank"
            ]
            [ span [ class "icon has-text-danger" ]
                [ i [ class "fab fa-youtube" ] [] ]
            ]
        , p []
            [ span [ style "padding-left" "5px" ]
                [ text link.name ]
            ]
        ]
