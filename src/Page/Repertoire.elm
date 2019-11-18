module Page.Repertoire exposing (Model, Msg(..), getLink, init, loadSongs, noSongSelected, organizeSongs, songDivId, update, view, viewLinkSection, viewLinkTable, viewSelectedSong, viewSongList)

import Browser.Navigation as Nav
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
import Models.Song exposing (Song, SongLinkSection, pitchToString, songDecoder, songModeToString)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), apiUrl, getRequest, notFoundView, scrollToElement, setToken, spinner)



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
    ( { common = common, songs = Loading, selected = selected }, Cmd.batch <| [ loadSongs common ] ++ toLoadSong )


songDivId : String
songDivId =
    "musicDeetsBox"



---- UPDATE ----


type Msg
    = OnLoadSongs (Result Http.Error (List Song))
    | OnLoadSong (Result Http.Error Song)
    | SelectSong Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadSongs (Ok songs) ->
            ( { model | songs = Loaded songs }, Cmd.none )

        OnLoadSongs (Err _) ->
            ( { model | songs = Failure }, Cmd.none )

        OnLoadSong (Ok song) ->
            ( { model | selected = Loaded song }, scrollToElement songDivId )

        OnLoadSong (Err _) ->
            ( { model | selected = Failure }, Cmd.none )

        SelectSong selected ->
            ( { model | selected = Loading }
            , Cmd.batch
                [ loadSong model.common selected
                , Route.replaceUrl model.common.key <| Route.Repertoire (Just selected)
                ]
            )



---- DATA ----


loadSongs : Common -> Cmd Msg
loadSongs common =
    getRequest common "/repertoire" (Http.expectJson OnLoadSongs <| Decode.list songDecoder)


loadSong : Common -> Int -> Cmd Msg
loadSong common songId =
    let
        url =
            "/repertoire/" ++ String.fromInt songId ++ "?details=true"
    in
    getRequest common url (Http.expectJson OnLoadSong songDecoder)


organizeSongs : List Song -> ( List Song, List Song )
organizeSongs songs =
    let
        sortedSongs =
            songs |> List.sortBy (\song -> song.title)

        currentSongs =
            sortedSongs |> List.filter (\song -> song.current)

        otherSongs =
            sortedSongs |> List.filter (\song -> not song.current)
    in
    ( currentSongs, otherSongs )


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
        songListBlock title content =
            div [ class "box" ]
                [ h1 [ class "title is-4" ] [ text title ]
                , content
                ]

        selectedSongBlock =
            case model.selected of
                NotAsked ->
                    noSongSelected

                Loading ->
                    spinner

                Loaded song ->
                    viewSelectedSong song

                Failure ->
                    text "whoops..."

        ( currentSongList, otherSongList ) =
            case model.songs of
                NotAsked ->
                    ( text "", text "" )

                Loading ->
                    ( spinner, spinner )

                Loaded songs ->
                    let
                        ( currentSongs, otherSongs ) =
                            organizeSongs songs

                        selectedSong =
                            case model.selected of
                                Loaded song ->
                                    Just song

                                _ ->
                                    Nothing
                    in
                    ( viewSongList selectedSong currentSongs
                    , viewSongList selectedSong otherSongs
                    )

                Failure ->
                    ( text "Oops I did", text "it again." )
    in
    div [ id "repertoire" ]
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-narrow" ]
                        [ songListBlock "Current" currentSongList
                        , songListBlock "Other" otherSongList
                        ]
                    , div [ class "column" ]
                        [ div
                            [ class "box"
                            , id songDivId
                            ]
                            [ selectedSongBlock ]
                        ]
                    ]
                ]
            ]
        ]


viewSongList : Maybe Song -> List Song -> Html Msg
viewSongList selectedSong songs =
    let
        isSelected song =
            isJust (selectedSong |> filter (\selected -> selected.id == song.id))

        songRow song =
            tr
                [ onClick <| SelectSong song.id
                , style "background-color"
                    (if isSelected song then
                        "#eeeeee"

                     else
                        ""
                    )
                ]
                [ td [] [ text song.title ] ]
    in
    table [ class "table is-fullwidth is-hoverable" ]
        [ tbody [] <| List.map songRow songs ]


noSongSelected : Html Msg
noSongSelected =
    p [] [ text "Select a song." ]


viewSelectedSong : Song -> Html Msg
viewSelectedSong song =
    let
        title =
            [ h1 [ class "title is-4" ] [ text song.title ] ]

        info =
            case song.info of
                Just songInfo ->
                    [ p [] [ text songInfo ], br [] [] ]

                Nothing ->
                    []

        mode =
            case song.mode of
                Just songMode ->
                    b [] [ text <| (" " ++ songModeToString songMode) ]

                Nothing ->
                    text ""

        key =
            [ p []
                [ text "Key: "
                , b [] [ text (song.key |> Maybe.map pitchToString |> Maybe.withDefault "?") ]
                , mode
                ]
            ]

        startingPitch =
            [ p []
                [ text "Starting pitch: "
                , b [] [ text <| (song.startingPitch |> Maybe.map pitchToString |> Maybe.withDefault "?") ]
                ]
            ]
    in
    div [] <|
        List.concat
            [ title
            , info
            , key
            , startingPitch
            , [ br [] [] ]
            , [ viewLinkTable (song.links |> Maybe.withDefault []) ]
            ]


viewLinkTable : List SongLinkSection -> Html Msg
viewLinkTable linkSections =
    table [ class "table is-fullwidth" ] <| List.map viewLinkSection linkSections


viewLinkSection : SongLinkSection -> Html Msg
viewLinkSection linkSection =
    let
        sheetMusicLink link =
            a [ class "button is-outlined is-primary", href <| getLink "pdf" link.target ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-scroll" ] [] ]
                , span [] [ text link.name ]
                ]

        midiLink link =
            a [ class "button is-outlined is-primary", href <| getLink "midi" link.target ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-volume-up" ] [] ]
                , span [] [ text link.name ]
                ]

        videoLink link =
            span [ style "display" "flex", style "align-items" "center" ]
                [ span [ class "icon has-text-grey-lighter", style "margin-right" ".5rem" ]
                    [ i [ class "fas fa-external-link-alt" ] [] ]
                , a [ class "button", href <| getLink "performance" link.target, target "_blank" ]
                    [ span [ class "icon has-text-danger" ] [ i [ class "fab fa-youtube" ] [] ] ]
                , p [] [ span [ style "padding-left" "5px" ] [ text link.name ] ]
                ]

        viewSection =
            case ( linkSection.name, List.isEmpty linkSection.links ) of
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
                [ td [ style "border" "none" ] [ text linkSection.name ]
                , td [ style "border" "none" ] <| (List.map viewer linkSection.links |> List.intersperse (text " "))
                ]

        Nothing ->
            tr [ style "display" "none" ] []
