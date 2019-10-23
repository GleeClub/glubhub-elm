module Page.Repertoire exposing (Model, Msg(..), getLink, init, loadSongs, organizeSongs, songDivId, update)

import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, img, input, label, section, span, text)
import Html.Attributes exposing (class, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (find)
import MD5
import Maybe.Extra exposing (filter, isJust)
import Models.Member exposing (Member, memberDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), apiUrl, getRequest, notFoundView, setToken, spinner)



---- MODEL ----


type alias Model =
    { common : Common
    , songs : RemoteData (List Song)
    , selected : Maybe Int
    }


init : Common -> Maybe Int -> ( Model, Cmd Msg )
init common selected =
    ( { common = common, songs = Loading, selected = selected }, loadSongs common )


songDivId : String
songDivId =
    "musicDeetsBox"



---- UPDATE ----


type Msg
    = OnLoadSongs (Result Http.Error (List Song))
    | SelectSong Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadSongs (Ok songs) ->
            ( { model | songs = Loaded songs }, Cmd.none )

        OnLoadSongs (Err _) ->
            ( { model | songs = Failure }, Cmd.none )

        SelectSong selected ->
            ( { model | selected = selected }, scrollToElement songDivId )



---- DATA ----


loadSongs : Common -> Cmd Msg
loadSongs common =
    getRequest common "/repertoire" (Http.expectJson <| Decode.list songDecoder)


organizeSongs : List Song -> ( List Song, ListSong )
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

        ( currentSongList, otherSongList, selectedSongBlock ) =
            case model.songs of
                NotAsked ->
                    ( text "", text "", text "" )

                Loading ->
                    ( spinner, spinner, spinner )

                Loaded songs ->
                    let
                        ( currentSongs, otherSongs ) =
                            organizeSongs songs

                        selectedSong =
                            model.selected |> Maybe.map (\songId -> songs |> find (\song -> song.id == songId))
                    in
                    ( viewSongList model.selectedSong currentSongs
                    , viewSongList model.selectedSong otherSongs
                    , viewSelectedSong selectedSong
                    )

                Error err ->
                    ( text "Oops", text "I did", text "it again." )
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
                        [ div [ class "box", id songDivId ] [ selectedSongBlock ] ]
                    ]
                ]
            ]
        ]


viewSongList : Maybe Int -> List Song -> Html Msg
viewSongList selectedId songs =
    let
        isSelected song =
            isJust (selectedId |> filter (\id -> id == song.id))

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
                Just info ->
                    [ p [] [ text info ], br [] [] ]

                Nothing ->
                    []

        mode =
            case song.mode of
                Just mode ->
                    b [] [ text mode ]

                Nothing ->
                    text ""

        key =
            [ p []
                [ text "Key:"
                , b [] [ text <| Maybe.withDefault "?" song.key ]
                , mode
                ]
            ]

        startingPitch =
            [ p []
                [ text "Starting pitch:"
                , b [] [ text <| Maybe.withDefault "?" song.startingPitch ]
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
            , [ viewLinkTable <| Maybe.withDefault [] song.linkSections ]
            ]


viewLinkTable : List SongLinkSection -> Html Msg
viewLinkTable linkSections =
    table [ class "table is-fullwidth" ] <| List.map viewLinkSection linkSections


viewLinkSection : SongLinkSection -> Maybe (Html Msg)
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
                , a [ class "button", href <| getLink "video" link.target, target "_blank" ]
                    [ span [ class "icon has-text-danger" ] [ i [ class "fab fa-youtube" ] [] ] ]
                , p [] [ span [] [ text link.name ] ]
                ]

        viewSection =
            case linkSection.name of
                "Sheet Music" ->
                    Just sheetMusicLink

                "MIDIs" ->
                    Just midiLink

                "Performances" ->
                    Just videoLink

                _ ->
                    Nothing
    in
    case viewSection of
        Just viewer ->
            tr []
                [ td [ style "border" "none" ] [ text linkSection.name ]
                , td [] <| List.map viewer linkSection.links
                ]

        Nothing ->
            tr [ style "display" "none" ] []
