module Page.Events.Setlist exposing (viewEventSetlist)

import Html exposing (Html, div, tbody, text)
import Models.Song exposing (Song, pitchToString, songDecoder)
import Route exposing (Route)
import Utils exposing (RemoteData(..), spinner)



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


viewEventSetlist : RemoteData (List Song) -> Html Msg
viewEventSetlist setlist =
    let
        content =
            case setlist of
                NotRequestedYet ->
                    text ""

                Loading ->
                    spinner

                Loaded songs ->
                    viewSongTable songs

                Failure ->
                    viewError
    in
    div [ id "setlist" ] [ content ]


viewSongTable : List Song -> Html Msg
viewSongTable songs =
    if List.length songs == 0 then
        div [] [ text "No set list for this event." ]

    else
        table [ class "table is-fullwidth is-striped" ]
            [ tbody [] songs |> List.indexedMap viewSongRow ]


viewSongRow : Int -> Song -> Html Msg
viewSongRow index song =
    tr [ key <| toString song.id ]
        [ td [] [ text <| toString song.id ]
        , td [ Route.href <| Route.Repertoire (Just song.id) ]
            [ text song.title ]
        , td []
            [ text song.key
                |> Maybe.map pitchToString
                |> Maybe.withDefault "No key"
            ]
        , td []
            [ text song.startingPitch
                |> Maybe.map pitchToString
                |> Maybe.withDefault "No starting pitch"
            ]
        ]
