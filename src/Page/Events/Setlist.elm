module Page.Events.Setlist exposing (Model, Msg(..), init, loadSetlist, update, view, viewSongRow, viewSongTable)

import Html exposing (Html, div, table, tbody, td, text, tr)
import Html.Attributes exposing (class, id)
import Http
import Json.Decode as Decode
import Models.Song exposing (Song, pitchToString, songDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), getRequest, spinner)



---- MODEL ----


type alias Model =
    { songs : RemoteData (List Song)
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { songs = Loading }, loadSetlist common eventId )



---- UPDATE ----


type Msg
    = OnLoadSetlist (Result Http.Error (List Song))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadSetlist (Ok songs) ->
            ( { model | songs = Loaded songs }, Cmd.none )

        OnLoadSetlist (Err _) ->
            ( { model | songs = Failure }, Cmd.none )



---- DATA ----


loadSetlist : Common -> Int -> Cmd Msg
loadSetlist common eventId =
    let
        url =
            "/events/" ++ String.fromInt eventId ++ "/setlist"
    in
    getRequest common url (Http.expectJson OnLoadSetlist <| Decode.list songDecoder)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        content =
            case model.songs of
                NotAsked ->
                    text ""

                Loading ->
                    spinner

                Loaded songs ->
                    viewSongTable songs

                Failure ->
                    text "whoops"
    in
    div [ id "setlist" ] [ content ]


viewSongTable : List Song -> Html Msg
viewSongTable songs =
    if List.length songs == 0 then
        div [] [ text "No set list for this event." ]

    else
        table [ class "table is-fullwidth is-striped" ]
            [ tbody [] (songs |> List.indexedMap viewSongRow) ]


viewSongRow : Int -> Song -> Html Msg
viewSongRow index song =
    tr []
        [ td [] [ text <| String.fromInt (index + 1) ]
        , td [ Route.href <| Route.Repertoire (Just song.id) ]
            [ text song.title ]
        , td []
            [ text
                (song.key
                    |> Maybe.map pitchToString
                    |> Maybe.withDefault "No key"
                )
            ]
        , td []
            [ text
                (song.startingPitch
                    |> Maybe.map pitchToString
                    |> Maybe.withDefault "No starting pitch"
                )
            ]
        ]
