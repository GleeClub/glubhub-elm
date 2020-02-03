module Page.Events.Attendance exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Forms as Forms exposing (checkboxInput, textInput)
import Error exposing (GreaseResult)
import Html exposing (Html, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List exposing (groupWhile)
import Models.Event exposing (EventAttendee, SimpleAttendance, eventAttendeeDecoder)
import Task
import Utils
    exposing
        ( Common
        , RemoteData(..)
        , SubmissionState(..)
        , fullName
        , getRequest
        , mapLoaded
        , postRequest
        , resultToRemote
        , resultToSubmissionState
        )



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
            ( { model
                | state = Sending
                , attendance =
                    model.attendance
                        |> mapLoaded
                            (List.setIf
                                (\a -> a.member.email == attendee.member.email)
                                attendee
                            )
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
        , Basics.submissionStateBox model.state
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

        columns =
            [ text (attendee.member |> fullName)
            , checkboxInput
                { content = ""
                , isChecked = attendee.attendance.didAttend
                , onChange =
                    \didAttend ->
                        UpdateAttendance
                            { attendee | attendance = { attendance | didAttend = didAttend } }
                }
            , checkboxInput
                { content = ""
                , isChecked = attendee.attendance.shouldAttend
                , onChange =
                    \shouldAttend ->
                        UpdateAttendance
                            { attendee | attendance = { attendance | shouldAttend = shouldAttend } }
                }
            , checkboxInput
                { content = ""
                , isChecked = attendee.attendance.confirmed
                , onChange =
                    \confirmed ->
                        UpdateAttendance
                            { attendee | attendance = { attendance | confirmed = confirmed } }
                }
            , textInput Forms.int
                { value = Just attendee.attendance.minutesLate
                , onInput =
                    \minutesLate ->
                        UpdateAttendance
                            { attendee | attendance = { attendance | minutesLate = minutesLate |> Maybe.withDefault 0 } }
                , attrs = []
                }
            ]
    in
    tr [ class "no-bottom-border" ]
        (columns |> List.map (\column -> td [] [ column ]))
