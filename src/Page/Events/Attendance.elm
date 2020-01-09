module Page.Events.Attendance exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, div, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onCheck, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (groupWhile)
import Models.Event exposing (EventAttendee, SimpleAttendance, eventAttendeeDecoder)
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), fullName, getRequest, mapLoaded, postRequest, resultToRemote, resultToSubmissionState)



---- MODEL ----


type alias Model =
    { common : Common
    , eventId : Int
    , state : SubmissionState
    , attendance : RemoteData (List EventAttendee)
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { common = common
      , eventId = eventId
      , state = NotSentYet
      , attendance = Loading
      }
    , loadAttendees common eventId
    )



---- UPDATE ----


type Msg
    = OnLoadAttendance (GreaseResult (List EventAttendee))
    | UpdateAttendance EventAttendee
    | OnUpdateAttendance (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadAttendance result ->
            ( { model | attendance = resultToRemote result }, Cmd.none )

        OnUpdateAttendance result ->
            ( { model | state = resultToSubmissionState result }, Cmd.none )

        UpdateAttendance attendee ->
            let
                attendanceMapper a =
                    if a.member.email == attendee.member.email then
                        attendee

                    else
                        a
            in
            ( { model
                | state = Sending
                , attendance = model.attendance |> mapLoaded (List.map attendanceMapper)
              }
            , updateAttendance model.common model.eventId attendee
            )



---- DATA ----


loadAttendees : Common -> Int -> Cmd Msg
loadAttendees common eventId =
    let
        url =
            "/events/" ++ String.fromInt eventId ++ "/see_whos_attending"
    in
    getRequest common url (Decode.list eventAttendeeDecoder)
        |> Task.attempt OnLoadAttendance


updateAttendance : Common -> Int -> EventAttendee -> Cmd Msg
updateAttendance common eventId attendee =
    let
        url =
            "/events/"
                ++ String.fromInt eventId
                ++ "/attendance/"
                ++ attendee.member.email
    in
    postRequest common url (serializeAttendance attendee.attendance)
        |> Task.attempt OnUpdateAttendance


serializeAttendance : SimpleAttendance -> Encode.Value
serializeAttendance attendance =
    Encode.object
        [ ( "shouldAttend", Encode.bool attendance.shouldAttend )
        , ( "didAttend", Encode.bool attendance.didAttend )
        , ( "confirmed", Encode.bool attendance.confirmed )
        , ( "minutesLate", Encode.int attendance.minutesLate )
        ]


groupAttendees : List EventAttendee -> List ( String, List EventAttendee )
groupAttendees attendees =
    let
        sortAttendees =
            List.sortBy (\attendee -> attendee.member.section |> Maybe.withDefault "ZZZ")

        groupedAttendees =
            attendees
                |> sortAttendees
                |> groupWhile (\attendeeA attendeeB -> attendeeA.member.section == attendeeB.member.section)

        buildSection attendeeSection =
            let
                allInSection =
                    Tuple.first attendeeSection :: Tuple.second attendeeSection

                sectionName =
                    (Tuple.first attendeeSection).member.section |> Maybe.withDefault "Homeless"
            in
            ( sectionName, sortAttendees allInSection )
    in
    groupedAttendees
        |> List.map buildSection



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ model.attendance |> Basics.remoteContent sectionTables
        , model.state |> Basics.submissionStateBox
        ]


sectionTables : List EventAttendee -> Html Msg
sectionTables attendees =
    table [ class "table is-fullwidth" ]
        (attendees
            |> groupAttendees
            |> List.concatMap sectionPartialTable
        )


sectionPartialTable : ( String, List EventAttendee ) -> List (Html Msg)
sectionPartialTable ( sectionName, attendees ) =
    [ sectionHeader sectionName
    , tbody [] (attendees |> List.map attendeeRow)
    ]


sectionHeader : String -> Html Msg
sectionHeader sectionName =
    let
        columnNames =
            [ sectionName
            , "Did Attend"
            , "Should Attend"
            , "Confirmed"
            , "Minutes Late"
            ]
    in
    thead []
        [ tr []
            (columnNames
                |> List.map (\name -> th [] [ text name ])
            )
        ]


attendeeRow : EventAttendee -> Html Msg
attendeeRow attendee =
    let
        attendance =
            attendee.attendance
    in
    tr [ class "no-bottom-border" ]
        [ td [] [ text (attendee.member |> fullName) ]
        , td []
            [ input
                [ type_ "checkbox"
                , checked attendee.attendance.didAttend
                , onCheck
                    (\didAttend ->
                        UpdateAttendance
                            { attendee | attendance = { attendance | didAttend = didAttend } }
                    )
                ]
                []
            ]
        , td []
            [ input
                [ type_ "checkbox"
                , checked attendee.attendance.shouldAttend
                , onCheck
                    (\shouldAttend ->
                        UpdateAttendance
                            { attendee | attendance = { attendance | shouldAttend = shouldAttend } }
                    )
                ]
                []
            ]
        , td []
            [ input
                [ type_ "checkbox"
                , checked attendee.attendance.confirmed
                , onCheck
                    (\confirmed ->
                        UpdateAttendance
                            { attendee | attendance = { attendance | confirmed = confirmed } }
                    )
                ]
                []
            ]
        , td []
            [ input
                [ type_ "number"
                , class "input"
                , value <| String.fromInt attendee.attendance.minutesLate
                , onInput
                    (\minutesLate ->
                        UpdateAttendance
                            { attendee | attendance = { attendance | minutesLate = String.toInt minutesLate |> Maybe.withDefault 0 } }
                    )
                ]
                []
            ]
        ]
