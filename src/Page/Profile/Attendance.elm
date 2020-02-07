module Page.Profile.Attendance exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Forms as Forms exposing (checkboxInput, textInput)
import Datetime
import Error exposing (GreaseError, GreaseResult)
import Html exposing (Html, a, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Event exposing (Event, Grades, Member, SimpleAttendance, gradesDecoder)
import Models.Info exposing (Enrollment(..))
import Request
import Route
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), mapLoaded, resultToRemote)



---- MODEL ----


type alias Model =
    { common : Common
    , member : Member
    , grades : RemoteData Grades
    , state : SubmissionState
    }


init : Common -> Member -> ( Model, Cmd Msg )
init common member =
    ( { common = common
      , member = member
      , grades = Loading
      , state = NotSentYet
      }
    , loadAttendance common member
        |> Task.attempt OnLoadAttendance
    )



---- UPDATE ----


type Msg
    = OnLoadAttendance (GreaseResult Grades)
    | UpdateEventAttendance Int SimpleAttendance
    | OnUpdateEventAttendance (GreaseResult Grades)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadAttendance result ->
            ( { model | grades = resultToRemote result }, Cmd.none )

        UpdateEventAttendance eventId attendance ->
            let
                gradesMapper grades =
                    { grades
                        | eventsWithChanges =
                            grades.eventsWithChanges
                                |> List.map gradeChangeMapper
                    }

                gradeChangeMapper event =
                    if event.id == eventId then
                        { event | attendance = Just attendance }

                    else
                        event
            in
            ( { model
                | state = Sending
                , grades = model.grades |> mapLoaded gradesMapper
              }
            , updateAttendance model.common eventId model.member attendance
            )

        OnUpdateEventAttendance (Ok grades) ->
            ( { model | grades = Loaded grades, state = NotSentYet }, Cmd.none )

        OnUpdateEventAttendance (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )



---- DATA ----


loadAttendance : Common -> Member -> Task.Task GreaseError Grades
loadAttendance common member =
    let
        url =
            "/members/" ++ member.email ++ "?grades=true"
    in
    Request.get common url (Decode.field "grades" gradesDecoder)


updateAttendance : Common -> Int -> Member -> SimpleAttendance -> Cmd Msg
updateAttendance common eventId member attendance =
    let
        url =
            "/events/" ++ String.fromInt eventId ++ "/attendance/" ++ member.email
    in
    Request.post common url (serializeAttendance attendance)
        |> Task.andThen (\_ -> loadAttendance common member)
        |> Task.attempt OnUpdateEventAttendance


serializeAttendance : SimpleAttendance -> Encode.Value
serializeAttendance attendance =
    Encode.object
        [ ( "shouldAttend", Encode.bool attendance.shouldAttend )
        , ( "didAttend", Encode.bool attendance.didAttend )
        , ( "confirmed", Encode.bool attendance.confirmed )
        , ( "minutesLate", Encode.int attendance.minutesLate )
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ model.grades
            |> Basics.remoteContent (profileAttendance model.common)
        , Basics.submissionStateBox model.state
        ]


profileAttendance : Common -> Grades -> Html Msg
profileAttendance common grades =
    let
        headerRow =
            tr []
                ([ "Date"
                 , "Event"
                 , "Type"
                 , "Should Attend?"
                 , "Did Attend?"
                 , "Mins Late"
                 , "Point Change"
                 , "Partial Score"
                 , "Rationale"
                 ]
                    |> List.map (\column -> th [] [ text column ])
                )

        eventWithChangesRow change =
            tr [ class "no-bottom-border" ]
                (eventWithChangesRowValues change
                    |> List.map (\cell -> td [] [ cell ])
                )

        eventWithChangesRowValues : Event -> List (Html Msg)
        eventWithChangesRowValues event =
            let
                attendance =
                    event.attendance
                        |> Maybe.withDefault
                            { shouldAttend = False
                            , didAttend = False
                            , confirmed = False
                            , minutesLate = 0
                            }
            in
            [ text (event.callTime |> Datetime.dateFormatter common.timeZone)
            , a [ Route.href <| Route.Events { id = Just event.id, tab = Nothing } ] [ text event.name ]
            , text event.type_
            , checkboxInput
                { content = ""
                , isChecked = attendance.shouldAttend
                , onChange =
                    \shouldAttend ->
                        UpdateEventAttendance event.id { attendance | shouldAttend = shouldAttend }
                }
            , checkboxInput
                { content = ""
                , isChecked = attendance.didAttend
                , onChange =
                    \didAttend ->
                        UpdateEventAttendance event.id { attendance | didAttend = didAttend }
                }
            , textInput Forms.int
                { value = Just attendance.minutesLate
                , onInput =
                    \minutesLate ->
                        UpdateEventAttendance event.id
                            { attendance | minutesLate = minutesLate |> Maybe.withDefault 0 }
                , attrs = [ Forms.Placeholder "0" ]
                }
            , text
                (event.gradeChange
                    |> Maybe.map (.change >> String.fromFloat)
                    |> Maybe.withDefault ""
                )
            , text
                (event.gradeChange
                    |> Maybe.map (.partialScore >> String.fromFloat)
                    |> Maybe.withDefault ""
                )
            , text
                (event.gradeChange
                    |> Maybe.map .reason
                    |> Maybe.withDefault ""
                )
            ]
    in
    table [ class "table" ]
        [ thead []
            [ headerRow ]
        , tbody []
            (grades.eventsWithChanges |> List.map eventWithChangesRow)
        ]
