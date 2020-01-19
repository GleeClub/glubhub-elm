module Page.Repertoire exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.SelectableList exposing (selectableListFull)
import Error exposing (GreaseResult)
import Html exposing (Html, a, div, p, section, td, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Song exposing (Pitch, Song, halfStepsAboveA, songDecoder)
import Page.Repertoire.EditSong as EditSong
import Page.Repertoire.SongSidebar exposing (songSidebar)
import Permissions
import Route
import Task
import Utils
    exposing
        ( Common
        , RemoteData(..)
        , SubmissionState(..)
        , decodeId
        , getRequest
        , mapLoaded
        , playPitch
        , postRequestFull
        , remoteToMaybe
        , resultToRemote
        , scrollToElement
        )



---- MODEL ----


type alias Model =
    { common : Common
    , songs : RemoteData (List Song)
    , selected : RemoteData ( Song, Maybe EditSong.Model )
    , createSongState : SubmissionState
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
    ( { common = common
      , songs = Loading
      , selected = selected
      , createSongState = NotSentYet
      }
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
    | OpenEditSong
    | CloseEditSong
    | EditSongMsg EditSong.InternalMsg
    | PropagateUpdateSong Song
    | UnselectSong
    | PlayPitch Pitch
    | CreateSong
    | OnCreateSong (GreaseResult Song)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadSongs songsResult ->
            ( { model | songs = resultToRemote songsResult }, Cmd.none )

        OnLoadSong (Ok song) ->
            ( { model | selected = Loaded ( song, Nothing ) }, scrollToElement songDivId )

        OnLoadSong (Err error) ->
            ( { model | selected = Failure error }, Cmd.none )

        SelectSong selected ->
            ( { model | selected = Loading }
            , Cmd.batch
                [ loadSong model.common selected
                , Route.replaceUrl model.common.key <| Route.Repertoire (Just selected)
                ]
            )

        OpenEditSong ->
            case model.selected of
                Loaded ( song, Nothing ) ->
                    ( { model
                        | selected = Loaded ( song, Just (EditSong.init model.common song) )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CloseEditSong ->
            case model.selected of
                Loaded ( song, Just _ ) ->
                    ( { model
                        | selected = Loaded ( song, Nothing )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditSongMsg editSongMsg ->
            case model.selected of
                Loaded ( song, Just editSongModel ) ->
                    let
                        ( newModel, newMsg ) =
                            EditSong.update editSongMsg editSongModel
                    in
                    ( { model
                        | selected = Loaded ( song, Just newModel )
                      }
                    , Cmd.map editSongTranslator newMsg
                    )

                _ ->
                    ( model, Cmd.none )

        PropagateUpdateSong updatedSong ->
            ( { model
                | selected = model.selected |> mapLoaded (Tuple.mapFirst (\_ -> updatedSong))
                , songs =
                    model.songs
                        |> mapLoaded
                            (List.map
                                (\song ->
                                    if song.id == updatedSong.id then
                                        updatedSong

                                    else
                                        song
                                )
                            )
              }
            , Cmd.none
            )

        UnselectSong ->
            ( { model | selected = NotAsked }, Cmd.none )

        PlayPitch pitch ->
            ( model, playPitch (halfStepsAboveA pitch) )

        CreateSong ->
            ( { model | createSongState = Sending }, createSong model.common )

        OnCreateSong (Ok song) ->
            ( { model
                | createSongState = NotSentYet
                , songs = model.songs |> mapLoaded ((::) song)
                , selected = Loaded ( song, Just <| EditSong.init model.common song )
              }
            , Cmd.none
            )

        OnCreateSong (Err error) ->
            ( { model | createSongState = ErrorSending error }, Cmd.none )


editSongTranslator : EditSong.Translator Msg
editSongTranslator =
    EditSong.translator
        { onInternalMessage = EditSongMsg
        , onUpdateSong = PropagateUpdateSong
        }



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


createSong : Common -> Cmd Msg
createSong common =
    let
        body =
            Encode.object
                [ ( "title", Encode.string "New Song" )
                ]

        songCreator =
            Utils.postRequestFull common "/repertoire" body Utils.decodeId

        songUrl id =
            "/repertoire/" ++ String.fromInt id

        songLoader id =
            getRequest common (songUrl id) songDecoder
    in
    songCreator
        |> Task.andThen songLoader
        |> Task.attempt OnCreateSong



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
                    [ songList model "Current" (Just model.createSongState) currentSongsFilter
                    , songList model "A-G" Nothing otherAToGFilter
                    , songList model "H-P" Nothing otherHToPFilter
                    , songList model "Q-Z" Nothing otherQToZFilter
                    ]
                ]
            ]
        , case model.selected of
            Loaded ( _, Just editSongModel ) ->
                Basics.sidebar
                    { data = Loaded ()
                    , render =
                        \_ ->
                            div []
                                [ Basics.backTextButton "finish editing" CloseEditSong
                                , Html.map editSongTranslator (EditSong.view editSongModel)
                                ]
                    , close = UnselectSong
                    }

            _ ->
                songSidebar
                    { common = model.common
                    , song = model.selected |> mapLoaded Tuple.first
                    , close = UnselectSong
                    , edit = OpenEditSong
                    , playPitch = PlayPitch
                    }
        ]


songList : Model -> String -> Maybe SubmissionState -> (Song -> Bool) -> Html Msg
songList model title createSongState filter =
    div [ class "column is-one-quarter is-centered" ]
        [ Basics.title title
        , selectableListFull
            { listItems = model.songs |> mapLoaded (List.sortBy .title << List.filter filter)
            , render = \song -> [ td [] [ text song.title ] ]
            , onSelect = \song -> SelectSong song.id
            , messageIfEmpty = "Nothin' to see here, buddy."
            , isSelected =
                \song ->
                    model.selected
                        |> remoteToMaybe
                        |> Maybe.map (\( s, _ ) -> s.id == song.id)
                        |> Maybe.withDefault False
            , contentAtTop = text ""
            , contentAtBottom =
                case createSongState of
                    Just createState ->
                        Basics.renderIfHasPermission model.common Permissions.editRepertoire <|
                            div []
                                [ div
                                    [ class "field is-grouped is-grouped-centered"
                                    , style "padding-top" "5px"
                                    ]
                                    [ p [ class "control" ]
                                        [ a
                                            [ class <| "button is-primary" ++ Utils.isLoadingClass (createState == Sending)
                                            , onClick CreateSong
                                            ]
                                            [ text "+ Add New Song" ]
                                        ]
                                    ]
                                , case createState of
                                    ErrorSending error ->
                                        Basics.errorBox error

                                    _ ->
                                        text ""
                                ]

                    Nothing ->
                        text ""
            }
        ]
