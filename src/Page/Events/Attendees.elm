module Page.Events.Attendees exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, br, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Models.Event exposing (EventAttendee, eventAttendeeDecoder)
import Page.Events.Attendance exposing (groupAttendees)
import Request
import Task
import Utils exposing (Common, RemoteData(..), fullName, resultToRemote)



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
    Request.get common url (Decode.list eventAttendeeDecoder)
        |> Task.attempt OnLoadAttendees


type alias AttendeeGroups =
    { attending : List ( String, SplitByConfirmed )
    , notAttending : List ( String, SplitByConfirmed )
    }


type alias SplitByConfirmed =
    { confirmed : List EventAttendee
    , notConfirmed : List EventAttendee
    }


separateAttendees : List EventAttendee -> AttendeeGroups
separateAttendees attendees =
    let
        ( attending, notAttending ) =
            attendees |> List.partition (\attendee -> attendee.attendance.shouldAttend)

        splitByConfirmed =
            List.partition (\attendee -> attendee.attendance.confirmed)
                >> (\( c, nc ) -> { confirmed = c, notConfirmed = nc })
    in
    { attending = attending |> groupAttendees |> List.map (Tuple.mapSecond splitByConfirmed)
    , notAttending = notAttending |> groupAttendees |> List.map (Tuple.mapSecond splitByConfirmed)
    }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ model |> Basics.remoteContent attendeeTables ]


attendeeTables : List EventAttendee -> Html Msg
attendeeTables attendees =
    let
        separated =
            separateAttendees attendees
    in
    Basics.column
        [ Basics.centeredTitle "Attending"
        , attendeeTable separated.attending
        , Basics.centeredTitle "Not Attending"
        , attendeeTable separated.notAttending
        ]


attendeeTable : List ( String, SplitByConfirmed ) -> Html Msg
attendeeTable sections =
    table [ class "table is-fullwidth" ]
        (sections
            |> List.concatMap
                (\( name, { confirmed, notConfirmed } ) ->
                    [ thead []
                        [ tr []
                            [ th [] [ text name ]
                            , th [] [ text "Confirmed" ]
                            , th [] [ text "Not Confirmed" ]
                            ]
                        ]
                    , tbody []
                        [ tr []
                            [ td [ style "width" "12%" ] []
                            , td [ style "width" "44%" ] <| attendeeNameList confirmed
                            , td [ style "width" "44%" ] <| attendeeNameList notConfirmed
                            ]
                        ]
                    ]
                )
        )


attendeeNameList : List EventAttendee -> List (Html Msg)
attendeeNameList attendees =
    attendees
        |> List.map (\attendee -> text (attendee.member |> fullName))
        |> List.intersperse (br [] [])
