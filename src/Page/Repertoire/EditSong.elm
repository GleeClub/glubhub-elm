module Page.Repertoire.EditSong exposing (InternalMsg, Model, Msg, Translator, init, translator, update, view)

import Base64
import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.Forms as Forms exposing (checkboxInput, fileInput, selectInput, textInput, textareaInput)
import Error exposing (GreaseError, GreaseResult)
import File exposing (File)
import Html exposing (Html, br, div, form, h2, h3, li, text, ul)
import Html.Attributes exposing (class, name, style)
import Html.Events exposing (onSubmit)
import Json.Encode as Encode
import List.Extra as List
import Models.Song
    exposing
        ( Song
        , SongLink
        , SongMode(..)
        , allPitches
        , pitchEncoder
        , pitchFromUnicode
        , pitchToUnicode
        , songLinkDecoder
        , songModeEncoder
        , songModeFromString
        , songModeToString
        )
import Page.Repertoire.Links exposing (songLinkButtonWithDelete)
import Request
import Task exposing (Task)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), scrollToElement, submissionStateBoxId)



---- MODEL ----


type alias Model =
    { common : Common
    , song : Song
    , newLinks : NewLinks
    , state : SubmissionState
    }


type alias NewLinks =
    { sheetMusic : NewFileLink
    , midi : NewFileLink
    , performance : NewUrlLink
    }


type alias NewFileLink =
    { name : String
    , file : Maybe File
    }


emptyFileLink : NewFileLink
emptyFileLink =
    { name = ""
    , file = Nothing
    }


type alias NewUrlLink =
    { name : String
    , url : String
    }


emptyUrlLink : NewUrlLink
emptyUrlLink =
    { name = ""
    , url = ""
    }


emptyNewLinks : NewLinks
emptyNewLinks =
    { sheetMusic = emptyFileLink
    , midi = emptyFileLink
    , performance = emptyUrlLink
    }


init : Common -> Song -> Model
init common song =
    { common = common
    , song = song
    , newLinks = emptyNewLinks
    , state = NotSentYet
    }



---- UPDATE ----


type OutMsg
    = PropagateSongUpdate Song


type Msg
    = ForSelf InternalMsg
    | ForParent OutMsg


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onUpdateSong : Song -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMessage, onUpdateSong } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal

        ForParent (PropagateSongUpdate song) ->
            onUpdateSong song


type InternalMsg
    = UpdateSong Song
    | OnUpdateSong (GreaseResult (Maybe SongLink))
    | UpdateNewLinks NewLinks
    | SelectSheetMusicFile (Maybe File)
    | AddSheetMusic
    | SelectMidiFile (Maybe File)
    | AddMidi
    | AddPerformance
    | DeleteSongLink Int


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSong updatedSong ->
            ( { model | state = Sending, song = updatedSong }
            , updateSong model.common updatedSong
            )

        OnUpdateSong (Ok maybeNewSongLink) ->
            let
                updatedModel =
                    maybeNewSongLink
                        |> Maybe.map (addLinkToSong model)
                        |> Maybe.withDefault model

                parentMsg =
                    ForParent <| PropagateSongUpdate updatedModel.song
            in
            ( { updatedModel | state = NotSentYet }
            , Task.perform identity (Task.succeed parentMsg)
            )

        OnUpdateSong (Err error) ->
            ( { model | state = ErrorSending error }
            , scrollToElement submissionStateBoxId
            )

        UpdateNewLinks newLinks ->
            ( { model | newLinks = newLinks }, Cmd.none )

        SelectSheetMusicFile file ->
            let
                sheetMusic =
                    model.newLinks.sheetMusic

                newLinks =
                    model.newLinks
            in
            ( { model
                | newLinks =
                    { newLinks
                        | sheetMusic = { sheetMusic | file = file }
                    }
              }
            , Cmd.none
            )

        AddSheetMusic ->
            addSheetMusic model

        SelectMidiFile file ->
            let
                midi =
                    model.newLinks.midi

                newLinks =
                    model.newLinks
            in
            ( { model
                | newLinks =
                    { newLinks
                        | midi = { midi | file = file }
                    }
              }
            , Cmd.none
            )

        AddMidi ->
            addMidi model

        AddPerformance ->
            addPerformance model

        DeleteSongLink linkId ->
            removeLinkFromSong model linkId


addSheetMusic : Model -> ( Model, Cmd Msg )
addSheetMusic model =
    case model.newLinks.sheetMusic.file of
        Just file ->
            ( { model | state = Sending }
            , uploadSheetMusic
                model.common
                model.song.id
                model.newLinks.sheetMusic.name
                file
            )

        _ ->
            ( model, Cmd.none )


addMidi : Model -> ( Model, Cmd Msg )
addMidi model =
    case model.newLinks.midi.file of
        Just file ->
            ( { model | state = Sending }
            , uploadMidi
                model.common
                model.song.id
                model.newLinks.midi.name
                file
            )

        _ ->
            ( model, Cmd.none )


addPerformance : Model -> ( Model, Cmd Msg )
addPerformance model =
    ( { model | state = Sending }
    , uploadPerformance
        model.common
        model.song.id
        model.newLinks.performance.name
        model.newLinks.performance.url
    )


addLinkToSong : Model -> SongLink -> Model
addLinkToSong model songLink =
    let
        song =
            model.song

        sectionMapper section =
            if section.name == songLink.type_ then
                { section | links = section.links ++ [ songLink ] }

            else
                section
    in
    { model
        | song = { song | links = song.links |> List.map sectionMapper }
        , newLinks = emptyNewLinks
    }


removeLinkFromSong : Model -> Int -> ( Model, Cmd Msg )
removeLinkFromSong model linkId =
    let
        song =
            model.song

        sectionMapper section =
            { section
                | links =
                    section.links
                        |> List.filter (\link -> link.id /= linkId)
            }
    in
    ( { model
        | state = Sending
        , song =
            { song
                | links =
                    song.links
                        |> List.map sectionMapper
            }
      }
    , deleteSongLink model.common linkId
    )



---- DATA ----


updateSong : Common -> Song -> Cmd Msg
updateSong common song =
    let
        body =
            serializeSong song

        url =
            "/repertoire/" ++ String.fromInt song.id
    in
    Request.post common url body
        |> Task.attempt (\result -> ForSelf <| OnUpdateSong (result |> Result.map (\_ -> Nothing)))


serializeSong : Song -> Encode.Value
serializeSong song =
    Encode.object
        [ ( "id", Encode.int song.id )
        , ( "title", Encode.string song.title )
        , ( "info", Encode.string (song.info |> Maybe.withDefault "") )
        , ( "current", Encode.bool song.current )
        , ( "key"
          , song.key
                |> Maybe.map pitchEncoder
                |> Maybe.withDefault Encode.null
          )
        , ( "startingPitch"
          , song.startingPitch
                |> Maybe.map pitchEncoder
                |> Maybe.withDefault Encode.null
          )
        , ( "mode"
          , song.mode
                |> Maybe.map songModeEncoder
                |> Maybe.withDefault Encode.null
          )
        ]


loadSongLink : Common -> Int -> Task GreaseError SongLink
loadSongLink common linkId =
    let
        url =
            "/repertoire/links/" ++ String.fromInt linkId
    in
    Request.get common url songLinkDecoder


deleteSongLink : Common -> Int -> Cmd Msg
deleteSongLink common linkId =
    let
        url =
            "/repertoire/links/" ++ String.fromInt linkId
    in
    Request.delete common url
        |> Task.attempt
            (\result ->
                ForSelf <|
                    OnUpdateSong (result |> Result.map (\_ -> Nothing))
            )


uploadPerformance : Common -> Int -> String -> String -> Cmd Msg
uploadPerformance common songId name linkUrl =
    let
        body =
            Encode.object
                [ ( "type", Encode.string "Performances" )
                , ( "name", Encode.string name )
                , ( "target", Encode.string linkUrl )
                ]

        url =
            "/repertoire/" ++ String.fromInt songId ++ "/links"

        createLink =
            Request.postReturningId common url body
    in
    createLink
        |> Task.andThen (loadSongLink common)
        |> Task.attempt
            (\result ->
                ForSelf <|
                    OnUpdateSong (result |> Result.map Just)
            )


uploadSheetMusic : Common -> Int -> String -> File -> Cmd Msg
uploadSheetMusic =
    uploadLinkWithFile "Sheet Music"


uploadMidi : Common -> Int -> String -> File -> Cmd Msg
uploadMidi =
    uploadLinkWithFile "MIDIs"


uploadLinkWithFile : String -> Common -> Int -> String -> File -> Cmd Msg
uploadLinkWithFile type_ common songId linkName file =
    let
        body fileBody =
            Encode.object
                [ ( "type", Encode.string type_ )
                , ( "name", Encode.string linkName )
                , ( "target", fileBody )
                ]

        url =
            "/repertoire/" ++ String.fromInt songId ++ "/links"

        createLink =
            Request.postReturningId common url
    in
    encodeFile file
        |> Task.map body
        |> Task.andThen createLink
        |> Task.andThen (loadSongLink common)
        |> Task.attempt
            (\result ->
                ForSelf <|
                    OnUpdateSong (result |> Result.map Just)
            )


encodeFile : File -> Task x Encode.Value
encodeFile file =
    serializeFile file
        |> Task.map
            (\fileContent ->
                Encode.object
                    [ ( "path", Encode.string (File.name file) )
                    , ( "content", fileContent )
                    ]
            )


serializeFile : File -> Task x Encode.Value
serializeFile =
    File.toBytes
        >> Task.map
            (Base64.fromBytes
                >> Maybe.map Encode.string
                >> Maybe.withDefault Encode.null
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        song =
            model.song

        pitchFormType =
            { toString = Maybe.map pitchToUnicode >> Maybe.withDefault "?"
            , fromString = pitchFromUnicode
            , textType = Forms.Text
            }

        songModeFormType =
            { toString = Maybe.map songModeToString >> Maybe.withDefault "(no mode)"
            , fromString = songModeFromString
            , textType = Forms.Text
            }
    in
    div [] <|
        [ h2
            [ class "title is-4"
            , style "text-align" "center"
            ]
            [ text "Edit" ]
        , h3
            [ class "subtitle is-6"
            , style "text-align" "center"
            ]
            [ text model.song.title ]
        , textInput Forms.string
            { value = model.song.title
            , onInput = \title -> ForSelf <| UpdateSong { song | title = title }
            , attrs =
                [ Forms.Title "Name of song"
                , Forms.Placeholder "Happy Birthday in 12/17"
                , Forms.RequiredField True
                ]
            }
        , selectInput pitchFormType
            { values = Nothing :: (allPitches |> List.map Just)
            , selected = song.key
            , onInput = \pitch -> ForSelf <| UpdateSong { song | key = pitch }
            , attrs = [ Forms.Title "Tonic" ]
            }
        , selectInput songModeFormType
            { values = [ Nothing, Just Major, Just Minor ]
            , selected = song.mode
            , onInput = \songMode -> ForSelf <| UpdateSong { song | mode = songMode }
            , attrs = [ Forms.Title "Mode" ]
            }
        , selectInput pitchFormType
            { values = Nothing :: (allPitches |> List.map Just)
            , selected = song.startingPitch
            , onInput = \pitch -> ForSelf <| UpdateSong { song | startingPitch = pitch }
            , attrs = [ Forms.Title "Starting pitch (if different)" ]
            }
        , checkboxInput
            { content = "Current Repertoire"
            , isChecked = song.current
            , onChange = \current -> ForSelf <| UpdateSong { song | current = current }
            }
        , textareaInput
            { value = song.info |> Maybe.withDefault ""
            , onInput = \info -> ForSelf <| UpdateSong { song | info = Just info }
            , attrs =
                [ Forms.Title "Comments"
                , Forms.Placeholder "There are no soloists, communism wins!"
                ]
            }
        , ul [ style "list-style" "none", style "padding-bottom" "10px" ]
            ((editSheetMusic model
                |> List.map (\x -> li [] [ x ])
             )
                ++ (editMidis model
                        |> List.map (\x -> li [] [ x ])
                   )
                ++ (editPerformances model
                        |> List.map (\x -> li [] [ x ])
                   )
            )
        , Basics.submissionStateBox model.state
        ]


editSheetMusic : Model -> List (Html Msg)
editSheetMusic model =
    let
        newLinks =
            model.newLinks

        sheetMusic =
            newLinks.sheetMusic

        header =
            Basics.divider "Sheet Music"

        deletableLinks =
            model.song.links
                |> List.find (\section -> section.name == "Sheet Music")
                |> Maybe.map .links
                |> Maybe.withDefault []
                |> List.map (songLinkButtonWithDelete (\id -> ForSelf <| DeleteSongLink id))
                |> List.intersperse (br [] [])

        newSongLink =
            form [ onSubmit <| ForSelf AddSheetMusic ]
                [ textInput Forms.string
                    { value = sheetMusic.name
                    , onInput = \name -> ForSelf <| UpdateNewLinks { newLinks | sheetMusic = { sheetMusic | name = name } }
                    , attrs =
                        [ Forms.Title "Sheet music name"
                        , Forms.Placeholder "Happy Birthday"
                        , Forms.RequiredField True
                        ]
                    }
                , fileInput
                    { file = sheetMusic.file
                    , selectFile = ForSelf << SelectSheetMusicFile
                    , attrs = [ Forms.Title "Sheet music file" ]
                    }
                , Buttons.submit
                    { content = "Add sheet music"
                    , attrs = []
                    }
                ]
    in
    header :: deletableLinks ++ [ br [] [], newSongLink ]


editMidis : Model -> List (Html Msg)
editMidis model =
    let
        newLinks =
            model.newLinks

        midi =
            newLinks.midi

        header =
            Basics.divider "MIDI's"

        deletableLinks =
            model.song.links
                |> List.find (\section -> section.name == "MIDIs")
                |> Maybe.map .links
                |> Maybe.withDefault []
                |> List.map (songLinkButtonWithDelete (\id -> ForSelf <| DeleteSongLink id))
                |> List.intersperse (br [] [])

        newSongLink =
            form [ onSubmit <| ForSelf AddMidi ]
                [ textInput Forms.string
                    { value = midi.name
                    , onInput = \name -> ForSelf <| UpdateNewLinks { newLinks | midi = { midi | name = name } }
                    , attrs =
                        [ Forms.Title "MIDI name"
                        , Forms.Placeholder "Happy Birthday - B1"
                        , Forms.RequiredField True
                        ]
                    }
                , fileInput
                    { file = midi.file
                    , selectFile = ForSelf << SelectMidiFile
                    , attrs = [ Forms.Title "MIDI file" ]
                    }
                , Buttons.submit
                    { content = "Add MIDI"
                    , attrs = []
                    }
                ]
    in
    header :: deletableLinks ++ [ br [] [], newSongLink ]


editPerformances : Model -> List (Html Msg)
editPerformances model =
    let
        newLinks =
            model.newLinks

        performance =
            newLinks.performance

        header =
            Basics.divider "Performances"

        deletableLinks =
            model.song.links
                |> List.find (\section -> section.name == "Performances")
                |> Maybe.map .links
                |> Maybe.withDefault []
                |> List.map (songLinkButtonWithDelete (\id -> ForSelf <| DeleteSongLink id))
                |> List.intersperse (br [] [])

        newSongLink =
            form [ onSubmit <| ForSelf AddPerformance ]
                [ textInput Forms.string
                    { value = performance.name
                    , onInput = \name -> ForSelf <| UpdateNewLinks { newLinks | performance = { performance | name = name } }
                    , attrs =
                        [ Forms.Title "Performance name"
                        , Forms.Placeholder "Happy Birthday, live from New York!"
                        , Forms.RequiredField True
                        ]
                    }
                , textInput Forms.string
                    { value = performance.url
                    , onInput = \url -> ForSelf <| UpdateNewLinks { newLinks | performance = { performance | url = url } }
                    , attrs =
                        [ Forms.Title "Performance URL"
                        , Forms.Prefix "https://youtu.be/"
                        , Forms.Placeholder "dtER80sOjX4"
                        , Forms.RequiredField True
                        ]
                    }
                , Buttons.submit
                    { content = "Add performance"
                    , attrs = []
                    }
                ]
    in
    header :: deletableLinks ++ [ br [] [], newSongLink ]
