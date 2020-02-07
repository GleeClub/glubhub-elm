module Page.Repertoire exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.DeleteModal exposing (deleteModal)
import Components.SelectableList exposing (selectableListFull)
import Error exposing (GreaseResult)
import Html exposing (Html, div, i, p, section, td, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Models.Song exposing (Pitch, Song, halfStepsAboveA, songDecoder)
import Page.Repertoire.EditSong as EditSong
import Page.Repertoire.SongSidebar exposing (songSidebar)
import Permissions
import Request
import Route
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), mapLoaded, playPitch, remoteToMaybe, resultToRemote)



---- MODEL ----


type alias Model =
    { common : Common
    , songs : RemoteData (List Song)
    , selected : RemoteData ( Song, Maybe EditSong.Model )
    , createState : SubmissionState
    , deleteState : Maybe SubmissionState
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
      , createState = NotSentYet
      , deleteState = Nothing
      }
    , Cmd.batch <| loadSongs common :: toLoadSong
    )



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
    | TryToDeleteSong
    | CancelDeleteSong
    | DeleteSong
    | OnDeleteSong (GreaseResult Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadSongs songsResult ->
            ( { model | songs = resultToRemote songsResult }, Cmd.none )

        OnLoadSong (Ok song) ->
            ( { model | selected = Loaded ( song, Nothing ) }, Cmd.none )

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
            let
                songMapper song =
                    if song.id == updatedSong.id then
                        updatedSong

                    else
                        song
            in
            ( { model
                | selected = model.selected |> mapLoaded (Tuple.mapFirst (\_ -> updatedSong))
                , songs = model.songs |> mapLoaded (List.map songMapper)
              }
            , Cmd.none
            )

        UnselectSong ->
            ( { model | selected = NotAsked }
            , Route.replaceUrl model.common.key <| Route.Repertoire Nothing
            )

        PlayPitch pitch ->
            ( model, playPitch (halfStepsAboveA pitch) )

        CreateSong ->
            ( { model | createState = Sending }, createSong model.common )

        OnCreateSong (Ok song) ->
            ( { model
                | createState = NotSentYet
                , songs = model.songs |> mapLoaded ((::) song)
                , selected = Loaded ( song, Just <| EditSong.init model.common song )
              }
            , Route.replaceUrl model.common.key <| Route.Repertoire <| Just song.id
            )

        OnCreateSong (Err error) ->
            ( { model | createState = ErrorSending error }, Cmd.none )

        OnDeleteSong (Ok songId) ->
            ( { model
                | songs =
                    model.songs
                        |> mapLoaded (List.filterNot (\s -> s.id == songId))
                , deleteState = Nothing
                , selected = NotAsked
              }
            , Route.replaceUrl model.common.key <| Route.Repertoire Nothing
            )

        OnDeleteSong (Err error) ->
            ( { model | deleteState = Just <| ErrorSending error }, Cmd.none )

        TryToDeleteSong ->
            ( { model | deleteState = Just NotSentYet }, Cmd.none )

        CancelDeleteSong ->
            ( { model | deleteState = Nothing }, Cmd.none )

        DeleteSong ->
            case model.selected of
                Loaded ( song, _ ) ->
                    ( { model | deleteState = Just Sending }, deleteSong model.common song.id )

                _ ->
                    ( model, Cmd.none )


editSongTranslator : EditSong.Translator Msg
editSongTranslator =
    EditSong.translator
        { onInternalMessage = EditSongMsg
        , onUpdateSong = PropagateUpdateSong
        }



---- DATA ----


loadSongs : Common -> Cmd Msg
loadSongs common =
    Request.get common "/repertoire" (Decode.list songDecoder)
        |> Task.attempt OnLoadSongs


loadSong : Common -> Int -> Cmd Msg
loadSong common songId =
    let
        url =
            "/repertoire/" ++ String.fromInt songId ++ "?details=true"
    in
    Request.get common url songDecoder
        |> Task.attempt OnLoadSong


createSong : Common -> Cmd Msg
createSong common =
    let
        body =
            Encode.object
                [ ( "title", Encode.string "New Song" )
                ]

        songCreator =
            Request.postReturningId common "/repertoire" body

        songUrl id =
            "/repertoire/" ++ String.fromInt id

        songLoader id =
            Request.get common (songUrl id) songDecoder
    in
    songCreator
        |> Task.andThen songLoader
        |> Task.attempt OnCreateSong


deleteSong : Common -> Int -> Cmd Msg
deleteSong common songId =
    let
        url =
            "/repertoire/" ++ String.fromInt songId
    in
    Request.delete common url
        |> Task.attempt
            (\result ->
                OnDeleteSong
                    (result |> Result.map (\_ -> songId))
            )



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
    div []
        [ Basics.section
            [ Basics.container
                [ Basics.columns
                    [ songList model "Current" (Just model.createState) currentSongsFilter
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
                                [ Buttons.back
                                    { content = "finish editing"
                                    , onClick = CloseEditSong
                                    }
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
                    , tryToDelete = TryToDeleteSong
                    , playPitch = PlayPitch
                    }
        , case ( model.selected, model.deleteState ) of
            ( Loaded ( song, _ ), Just state ) ->
                deleteSongModal song state

            _ ->
                text ""
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
                createSongState
                    |> Maybe.map (createSongButton model.common)
                    |> Maybe.withDefault (text "")
            }
        ]


createSongButton : Common -> SubmissionState -> Html Msg
createSongButton common state =
    Basics.renderIfHasPermission common Permissions.editRepertoire <|
        div [ style "padding-top" "5px" ]
            [ Buttons.group
                { alignment = Buttons.AlignCenter
                , connected = False
                , buttons =
                    [ Buttons.button
                        { content = "+ Add New Song"
                        , onClick = Just CreateSong
                        , attrs =
                            [ Buttons.Color Buttons.IsPrimary
                            , Buttons.IsLoading (state == Sending)
                            ]
                        }
                    ]
                }
            , case state of
                ErrorSending error ->
                    Basics.errorBox error

                _ ->
                    text ""
            ]


deleteSongModal : Song -> SubmissionState -> Html Msg
deleteSongModal song state =
    deleteModal
        { title = "Are you sure you want to delete " ++ song.title ++ "?"
        , cancel = CancelDeleteSong
        , confirm = DeleteSong
        , state = state
        , content =
            div []
                [ p []
                    [ text <| "Are you sure you want to delete " ++ song.title ++ "?" ]
                , p []
                    [ i [] [ text "It'll be gong like Varys' dong." ] ]
                ]
        }
