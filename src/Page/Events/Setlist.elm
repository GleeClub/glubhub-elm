module Page.Events.Setlist exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, div, table, tbody, td, text, tr)
import Html.Attributes exposing (class, id)
import Json.Decode as Decode
import Models.Song exposing (Song, pitchToString, songDecoder)
import Route
import Task
import Utils exposing (Common, RemoteData(..), getRequest, resultToRemote)



---- MODEL ----


type alias Model =
    RemoteData (List Song)


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( Loading, loadSetlist common eventId )



---- UPDATE ----


type Msg
    = OnLoadSetlist (GreaseResult (List Song))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        OnLoadSetlist songsResult ->
            ( resultToRemote songsResult, Cmd.none )



---- DATA ----


loadSetlist : Common -> Int -> Cmd Msg
loadSetlist common eventId =
    let
        url =
            "/events/" ++ String.fromInt eventId ++ "/setlist"
    in
    getRequest common url (Decode.list songDecoder)
        |> Task.attempt OnLoadSetlist



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "setlist" ]
        [ model |> Basics.remoteContent viewSongTable ]


viewSongTable : List Song -> Html Msg
viewSongTable songs =
    if List.length songs == 0 then
        div [] [ text "No set list for this event." ]

    else
        table [ class "table is-striped" ]
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
