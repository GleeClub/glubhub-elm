module Page.Events.Attendees exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, br, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Models.Event exposing (EventAttendee, eventAttendeeDecoder)
import Task
import Utils exposing (Common, RemoteData(..), fullName, getRequest, resultToRemote)



---- MODEL ----


type alias Model =
    RemoteData (List EventAttendee)


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( Loading, loadAttendees common eventId )



---- UPDATE ----


type Msg
    = OnLoadAttendees (GreaseResult (List EventAttendee))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        OnLoadAttendees attendeesResult ->
            ( resultToRemote attendeesResult, Cmd.none )



---- DATA ----


loadAttendees : Common -> Int -> Cmd Msg
loadAttendees common eventId =
    let
        url =
            "/events/" ++ String.fromInt eventId ++ "/see_whos_attending"
    in
    getRequest common url (Decode.list eventAttendeeDecoder)
        |> Task.attempt OnLoadAttendees


separateAttendees :
    List EventAttendee
    -> ( ( List EventAttendee, List EventAttendee ), ( List EventAttendee, List EventAttendee ) )
separateAttendees attendees =
    let
        ( attending, notAttending ) =
            attendees |> List.partition (\attendee -> attendee.attendance.shouldAttend)

        splitByConfirmed =
            List.partition (\attendee -> attendee.attendance.confirmed)
    in
    ( splitByConfirmed attending, splitByConfirmed notAttending )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ model |> Basics.remoteContent attendeeTables ]


attendeeTables : List EventAttendee -> Html Msg
attendeeTables attendees =
    let
        ( attending, notAttending ) =
            separateAttendees attendees
    in
    Basics.column
        [ Basics.centeredTitle "Attending"
        , attendeeTable attending
        , Basics.centeredTitle "Not Attending"
        , attendeeTable notAttending
        ]


attendeeTable : ( List EventAttendee, List EventAttendee ) -> Html Msg
attendeeTable ( confirmed, notConfirmed ) =
    table [ class "table is-fullwidth" ]
        [ thead []
            [ tr []
                [ th [] [ text "Confirmed" ]
                , th [] [ text "Not Confirmed" ]
                ]
            ]
        , tbody []
            [ tr []
                [ td [ style "width" "50%" ] <| attendeeNameList confirmed
                , td [ style "width" "50%" ] <| attendeeNameList notConfirmed
                ]
            ]
        ]


attendeeNameList : List EventAttendee -> List (Html Msg)
attendeeNameList attendees =
    attendees
        |> List.map (\attendee -> text (attendee.member |> fullName))
        |> List.intersperse (br [] [])
