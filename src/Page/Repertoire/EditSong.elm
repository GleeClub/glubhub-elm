module Page.Repertoire.EditSong exposing (InternalMsg, Model, Msg, Translator, init, translator, update, view)

import Components.Basics as Basics
import Components.Forms exposing (checkboxInput, fileInput, selectInput, textInput, textInputWithPrefix, textareaInput)
import Error exposing (GreaseError, GreaseResult)
import File exposing (File)
import Html exposing (Html, br, button, div, form, h2, h3, li, text, ul)
import Html.Attributes exposing (class, name, style, type_)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Models.Song
    exposing
        ( Song
        , SongLink
        , SongMode(..)
        , allPitches
        , pitchDecoder
        , pitchEncoder
        , pitchToString
        , songLinkDecoder
        , songModeDecoder
        , songModeEncoder
        , songModeToString
        )
import Page.Repertoire.Links exposing (songLinkButtonWithDelete)
import Task exposing (Task)
import Utils
    exposing
        ( Common
        , RemoteData(..)
        , SubmissionState(..)
        , decodeId
        , deleteRequest
        , getRequest
        , postRequest
        , postRequestFull
        , scrollToElement
        , submissionStateBoxId
        )



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
    | SelectSheetMusicFile (List File)
    | AddSheetMusic
    | SelectMidiFile (List File)
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

        SelectSheetMusicFile files ->
            let
                sheetMusic =
                    model.newLinks.sheetMusic

                newLinks =
                    model.newLinks
            in
            ( { model
                | newLinks =
                    { newLinks
                        | sheetMusic =
                            { sheetMusic
                                | file = List.head files
                            }
                    }
              }
            , Cmd.none
            )

        AddSheetMusic ->
            addSheetMusic model

        SelectMidiFile files ->
            let
                midi =
                    model.newLinks.midi

                newLinks =
                    model.newLinks
            in
            ( { model
                | newLinks =
                    { newLinks
                        | midi =
                            { midi
                                | file = List.head files
                            }
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
            { section | links = section.links |> List.filter (\link -> link.id /= linkId) }
    in
    ( { model | state = Sending, song = { song | links = song.links |> List.map sectionMapper } }
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
    postRequest common url body
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
                |> Maybe.map (pitchEncoder >> Encode.string)
                |> Maybe.withDefault Encode.null
          )
        , ( "startingPitch"
          , song.startingPitch
                |> Maybe.map (pitchEncoder >> Encode.string)
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
    getRequest common url songLinkDecoder


deleteSongLink : Common -> Int -> Cmd Msg
deleteSongLink common linkId =
    let
        url =
            "/repertoire/links/" ++ String.fromInt linkId
    in
    deleteRequest common url
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
            postRequestFull common url body decodeId
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
        body =
            Encode.object
                [ ( "type", Encode.string type_ )
                , ( "name", Encode.string linkName )
                , ( "target", Encode.string (File.name file) )
                ]

        url =
            "/repertoire/" ++ String.fromInt songId ++ "/links"

        createLink =
            postRequestFull common url body decodeId
    in
    uploadFile common file
        |> Task.andThen (\_ -> createLink)
        |> Task.andThen (loadSongLink common)
        |> Task.attempt
            (\result ->
                ForSelf <|
                    OnUpdateSong (result |> Result.map Just)
            )


uploadFile : Common -> File -> Task GreaseError ()
uploadFile common file =
    let
        url =
            "/repertoire/upload"

        body fileContent =
            Encode.object
                [ ( "path", Encode.string (File.name file) )
                , ( "content", fileContent )
                ]
    in
    serializeFile file
        |> Task.andThen (\f -> postRequest common url (body f))


serializeFile : File -> Task x Encode.Value
serializeFile =
    let
        parseUrl =
            String.split "base64,"
                >> List.getAt 1
                >> Maybe.withDefault ""
                >> Encode.string
    in
    File.toUrl >> Task.map parseUrl



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        song =
            model.song

        renderPitch pitch =
            pitch
                |> Maybe.map (\p -> ( pitchEncoder p, pitchToString p ))
                |> Maybe.withDefault ( "", "?" )

        renderSongMode songMode =
            let
                str =
                    songMode
                        |> Maybe.map songModeToString
                        |> Maybe.withDefault "(no mode)"
            in
            ( str, str )

        parsePitch pitch =
            Decode.decodeString pitchDecoder ("\"" ++ pitch ++ "\"")
                |> Result.withDefault Nothing

        parseSongMode songMode =
            Decode.decodeString songModeDecoder ("\"" ++ songMode ++ "\"")
                |> Result.toMaybe

        pitchIsSelected currentPitch givenPitch =
            case ( currentPitch, givenPitch ) of
                ( Just currentKey, Just pitch ) ->
                    currentKey == pitch

                ( Nothing, Nothing ) ->
                    True

                _ ->
                    False
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
        , textInput
            { title = "Name of song"
            , helpText = Nothing
            , value = model.song.title
            , placeholder = "Happy Birthday in 12/17"
            , required = True
            , onInput = \title -> ForSelf <| UpdateSong { song | title = title }
            }
        , selectInput
            { title = "Tonic"
            , helpText = Nothing
            , loading = False
            , values = Nothing :: (allPitches |> List.map Just)
            , render = renderPitch
            , onSelect = \pitch -> ForSelf <| UpdateSong { song | key = parsePitch pitch }
            , selected = pitchIsSelected song.key
            }
        , selectInput
            { title = "Mode"
            , helpText = Nothing
            , loading = False
            , values = [ Nothing, Just Major, Just Minor ]
            , render = renderSongMode
            , onSelect = \songMode -> ForSelf <| UpdateSong { song | mode = parseSongMode songMode }
            , selected = (==) song.mode
            }
        , selectInput
            { title = "Starting pitch (if different)"
            , helpText = Nothing
            , loading = False
            , values = Nothing :: (allPitches |> List.map Just)
            , render = renderPitch
            , onSelect = \pitch -> ForSelf <| UpdateSong { song | startingPitch = parsePitch pitch }
            , selected = pitchIsSelected song.startingPitch
            }
        , checkboxInput
            { content = "Current Repertoire"
            , isChecked = song.current
            , onChange = \current -> ForSelf <| UpdateSong { song | current = current }
            }
        , textareaInput
            { title = "Comments"
            , helpText = Nothing
            , value = song.info |> Maybe.withDefault ""
            , placeholder = "There are no soloists, communism wins!"
            , required = False
            , onInput = \info -> ForSelf <| UpdateSong { song | info = Just info }
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
                [ textInput
                    { title = "Sheet music name"
                    , helpText = Nothing
                    , value = sheetMusic.name
                    , placeholder = "Happy Birthday - B1"
                    , required = True
                    , onInput = \name -> ForSelf <| UpdateNewLinks { newLinks | sheetMusic = { sheetMusic | name = name } }
                    }
                , fileInput
                    { title = "Sheet music file"
                    , helpText = Nothing
                    , file = sheetMusic.file
                    , selectFile = ForSelf << SelectSheetMusicFile
                    }
                , button
                    [ class "button", type_ "submit" ]
                    [ text "Add sheet music" ]
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
                [ textInput
                    { title = "MIDI name"
                    , helpText = Nothing
                    , value = midi.name
                    , placeholder = "Happy Birthday - B1"
                    , required = True
                    , onInput = \name -> ForSelf <| UpdateNewLinks { newLinks | midi = { midi | name = name } }
                    }
                , fileInput
                    { title = "MIDI file"
                    , helpText = Nothing
                    , file = midi.file
                    , selectFile = ForSelf << SelectMidiFile
                    }
                , button
                    [ class "button", type_ "submit" ]
                    [ text "Add MIDI" ]
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
                [ textInput
                    { title = "Performance name"
                    , helpText = Nothing
                    , value = performance.name
                    , placeholder = "Happy Birthday, live from New York!"
                    , required = True
                    , onInput = \name -> ForSelf <| UpdateNewLinks { newLinks | performance = { performance | name = name } }
                    }
                , textInputWithPrefix
                    { title = "Performance URL"
                    , prefix = "https://youtu.be/"
                    , helpText = Nothing
                    , value = performance.url
                    , placeholder = "dtER80sOjX4"
                    , required = True
                    , onInput = \url -> ForSelf <| UpdateNewLinks { newLinks | performance = { performance | url = url } }
                    }
                , button
                    [ class "button", type_ "submit" ]
                    [ text "Add performance" ]
                ]
    in
    header :: deletableLinks ++ [ br [] [], newSongLink ]
