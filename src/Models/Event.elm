module Models.Event exposing (AbsenceRequest, AbsenceRequestState(..), Attendance, Event, EventAttendee, EventCarpool, EventWithAttendance, FullEvent, FullEventAttendance, FullEventGig, Gig, GradeChange, Grades, HasAttendance, HasCallAndReleaseTimes, IsEvent, Member, MemberPermission, MemberRole, SimpleAttendance, UpdatedCarpool, absenceRequestDecoder, absenceRequestStateDecoder, attendanceDecoder, eventAttendeeDecoder, eventCarpoolDecoder, eventDecoder, eventWithAttendanceDecoder, fullEventAttendanceDecoder, fullEventDecoder, fullEventGigDecoder, gigDecoder, gradeChangeDecoder, gradesDecoder, memberDecoder, memberPermissionDecoder, memberRoleDecoder, simpleAttendanceDecoder)

import Json.Decode as Decode exposing (Decoder, bool, float, int, nullable, string)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Models.Info exposing (Enrollment, Role, Uniform, enrollmentDecoder, posixDecoder, roleDecoder, uniformDecoder)
import Time exposing (Posix)


type alias HasCallAndReleaseTimes a =
    { a
        | callTime : Posix
        , releaseTime : Maybe Posix
    }


type alias IsEvent a =
    HasCallAndReleaseTimes
        { a
            | id : Int
            , name : String
            , semester : String
            , type_ : String
            , points : Int
            , comments : Maybe String
            , location : Maybe String
            , gigCount : Bool
            , defaultAttend : Bool
            , section : Maybe String
        }


type alias HasAttendance a =
    { a
        | shouldAttend : Bool
        , didAttend : Bool
        , confirmed : Bool
        , minutesLate : Int
    }


type alias Event =
    { id : Int
    , name : String
    , semester : String
    , type_ : String
    , callTime : Posix
    , releaseTime : Maybe Posix
    , points : Int
    , comments : Maybe String
    , location : Maybe String
    , gigCount : Bool
    , defaultAttend : Bool
    , section : Maybe String
    , gig : Maybe Gig
    }


eventDecoder : Decoder Event
eventDecoder =
    Decode.succeed Event
        |> required "id" int
        |> required "name" string
        |> required "semester" string
        |> required "type" string
        |> required "callTime" posixDecoder
        |> optional "releaseTime" (nullable posixDecoder) Nothing
        |> required "points" int
        |> optional "comments" (nullable string) Nothing
        |> optional "location" (nullable string) Nothing
        |> required "gigCount" bool
        |> required "defaultAttend" bool
        |> optional "section" (nullable string) Nothing
        |> custom
            (Decode.value
                |> Decode.andThen
                    (\json ->
                        let
                            decoder : Decoder Int
                            decoder =
                                Decode.field "uniform" int
                        in
                        case Decode.decodeValue decoder json of
                            Err _ ->
                                Decode.succeed Nothing

                            Ok _ ->
                                Decode.andThen
                                    (\x ->
                                        Decode.succeed <| Just x
                                    )
                                    gigDecoder
                    )
            )


type alias FullEvent =
    { id : Int
    , name : String
    , semester : String
    , type_ : String
    , callTime : Posix
    , releaseTime : Maybe Posix
    , points : Int
    , comments : Maybe String
    , location : Maybe String
    , gigCount : Bool
    , defaultAttend : Bool
    , section : Maybe String
    , rsvpIssue : Maybe String
    , gig : Maybe FullEventGig
    , attendance : Maybe FullEventAttendance
    }


fullEventDecoder : Decoder FullEvent
fullEventDecoder =
    Decode.succeed FullEvent
        |> required "id" int
        |> required "name" string
        |> required "semester" string
        |> required "type" string
        |> required "callTime" posixDecoder
        |> optional "releaseTime" (nullable posixDecoder) Nothing
        |> required "points" int
        |> optional "comments" (nullable string) Nothing
        |> optional "location" (nullable string) Nothing
        |> required "gigCount" bool
        |> required "defaultAttend" bool
        |> optional "section" (nullable string) Nothing
        |> optional "rsvpIssue" (nullable string) Nothing
        |> custom
            (Decode.value
                |> Decode.andThen
                    (\json ->
                        let
                            decoder =
                                Decode.field "uniform" uniformDecoder
                        in
                        case Decode.decodeValue decoder json of
                            Err _ ->
                                Decode.succeed Nothing

                            Ok _ ->
                                fullEventGigDecoder
                                    |> Decode.andThen
                                        (\x ->
                                            Decode.succeed <| Just x
                                        )
                    )
            )
        |> custom
            (Decode.value
                |> Decode.andThen
                    (\json ->
                        let
                            decoder =
                                Decode.field "shouldAttend" bool
                        in
                        case Decode.decodeValue decoder json of
                            Err _ ->
                                Decode.succeed Nothing

                            Ok _ ->
                                fullEventAttendanceDecoder
                                    |> Decode.andThen
                                        (\x ->
                                            Decode.succeed <| Just x
                                        )
                    )
            )


type alias FullEventGig =
    { performanceTime : Posix
    , uniform : Uniform
    , contactName : Maybe String
    , contactEmail : Maybe String
    , contactPhone : Maybe String
    , price : Maybe Int
    , public : Bool
    , summary : Maybe String
    , description : Maybe String
    }


fullEventGigDecoder : Decoder FullEventGig
fullEventGigDecoder =
    Decode.succeed FullEventGig
        |> required "performanceTime" posixDecoder
        |> required "uniform" uniformDecoder
        |> optional "contactName" (nullable string) Nothing
        |> optional "contactEmail" (nullable string) Nothing
        |> optional "contactPhone" (nullable string) Nothing
        |> optional "price" (nullable int) Nothing
        |> required "public" bool
        |> optional "summary" (nullable string) Nothing
        |> optional "description" (nullable string) Nothing


type alias FullEventAttendance =
    { shouldAttend : Bool
    , didAttend : Bool
    , confirmed : Bool
    , minutesLate : Int
    , gradeChange : Maybe Float
    , gradeChangeReason : Maybe String
    , partialScore : Maybe Float
    }


fullEventAttendanceDecoder : Decoder FullEventAttendance
fullEventAttendanceDecoder =
    Decode.succeed FullEventAttendance
        |> required "shouldAttend" bool
        |> required "didAttend" bool
        |> required "confirmed" bool
        |> required "minutesLate" int
        |> optional "gradeChange" (nullable float) Nothing
        |> optional "gradeChangeReason" (nullable string) Nothing
        |> optional "partialScore" (nullable float) Nothing


type alias AbsenceRequest =
    { member : String
    , event : Int
    , time : Posix
    , reason : String
    , state : AbsenceRequestState
    }


absenceRequestDecoder : Decoder AbsenceRequest
absenceRequestDecoder =
    Decode.succeed AbsenceRequest
        |> required "member" string
        |> required "event" int
        |> required "time" posixDecoder
        |> required "reason" string
        |> required "state" absenceRequestStateDecoder


type AbsenceRequestState
    = Approved
    | Denied
    | Pending


absenceRequestStateDecoder : Decoder AbsenceRequestState
absenceRequestStateDecoder =
    string
        |> Decode.andThen
            (\x ->
                case x of
                    "approved" ->
                        Decode.succeed Approved

                    "denied" ->
                        Decode.succeed Denied

                    "pending" ->
                        Decode.succeed Pending

                    _ ->
                        Decode.fail "AbsenceRequestState can only be \"approved\", \"denied\", or \"pending\""
            )


type alias Gig =
    { performanceTime : Posix
    , uniform : Int
    , contactName : Maybe String
    , contactEmail : Maybe String
    , contactPhone : Maybe String
    , price : Maybe Int
    , public : Bool
    , summary : Maybe String
    , description : Maybe String
    }


gigDecoder : Decoder Gig
gigDecoder =
    Decode.succeed Gig
        |> required "performanceTime" posixDecoder
        |> required "uniform" int
        |> optional "contactName" (nullable string) Nothing
        |> optional "contactEmail" (nullable string) Nothing
        |> optional "contactPhone" (nullable string) Nothing
        |> optional "price" (nullable int) Nothing
        |> required "public" bool
        |> optional "summary" (nullable string) Nothing
        |> optional "description" (nullable string) Nothing


type alias Attendance =
    { member : String
    , event : Int
    , shouldAttend : Bool
    , didAttend : Bool
    , confirmed : Bool
    , minutesLate : Int
    }


attendanceDecoder : Decoder Attendance
attendanceDecoder =
    Decode.succeed Attendance
        |> required "member" string
        |> required "event" int
        |> required "shouldAttend" bool
        |> required "didAttend" bool
        |> required "confirmed" bool
        |> required "minutesLate" int


type alias EventAttendee =
    { member : Member
    , attendance : SimpleAttendance
    }


eventAttendeeDecoder : Decoder EventAttendee
eventAttendeeDecoder =
    Decode.succeed EventAttendee
        |> custom memberDecoder
        |> custom simpleAttendanceDecoder


type alias SimpleAttendance =
    { shouldAttend : Bool
    , didAttend : Bool
    , confirmed : Bool
    , minutesLate : Int
    }


simpleAttendanceDecoder : Decoder SimpleAttendance
simpleAttendanceDecoder =
    Decode.succeed SimpleAttendance
        |> required "shouldAttend" bool
        |> required "didAttend" bool
        |> required "confirmed" bool
        |> required "minutesLate" int


type alias EventWithAttendance =
    { event : FullEvent
    , attendance : SimpleAttendance
    , rsvpIssue : Maybe String
    }


eventWithAttendanceDecoder : Decoder EventWithAttendance
eventWithAttendanceDecoder =
    Decode.succeed EventWithAttendance
        |> custom fullEventDecoder
        |> custom simpleAttendanceDecoder
        |> optional "rsvpIssue" (nullable string) Nothing


type alias EventCarpool =
    { id : Int
    , event : Int
    , driver : Member
    , passengers : List Member
    }


eventCarpoolDecoder : Decoder EventCarpool
eventCarpoolDecoder =
    Decode.succeed EventCarpool
        |> required "id" int
        |> required "event" int
        |> required "driver" memberDecoder
        |> required "passengers" (Decode.list memberDecoder)


type alias UpdatedCarpool =
    { id : Maybe Int
    , driver : Member
    , passengers : List Member
    }


type alias Grades =
    { finalGrade : Float
    , volunteerGigsAttended : Int
    , gigRequirement : Int
    , semesterIsFinished : Bool
    , changes : List GradeChange
    }


gradesDecoder : Decoder Grades
gradesDecoder =
    Decode.succeed Grades
        |> required "finalGrade" float
        |> required "volunteerGigsAttended" int
        |> required "gigRequirement" int
        |> required "semesterIsFinished" bool
        |> required "changes" (Decode.list gradeChangeDecoder)


type alias GradeChange =
    { event : Event
    , attendance : Attendance
    , reason : String
    , change : Float
    , partialScore : Float
    }


gradeChangeDecoder : Decoder GradeChange
gradeChangeDecoder =
    Decode.succeed GradeChange
        |> required "event" eventDecoder
        |> required "attendance" attendanceDecoder
        |> required "reason" string
        |> required "change" float
        |> required "partialScore" float


type alias Member =
    { email : String
    , firstName : String
    , preferredName : Maybe String
    , lastName : String
    , phoneNumber : String
    , picture : Maybe String
    , passengers : Int
    , location : String
    , onCampus : Maybe Bool
    , about : Maybe String
    , major : Maybe String
    , minor : Maybe String
    , hometown : Maybe String
    , arrivedAtTech : Maybe Int
    , gatewayDrug : Maybe String
    , conflicts : Maybe String
    , dietaryRestrictions : Maybe String
    , section : Maybe String
    , enrollment : Maybe Enrollment
    , permissions : List MemberPermission
    , positions : List String
    , grades : Maybe Grades
    }


memberDecoder : Decoder Member
memberDecoder =
    Decode.succeed Member
        |> required "email" string
        |> required "firstName" string
        |> optional "preferredName" (nullable string) Nothing
        |> required "lastName" string
        |> required "phoneNumber" string
        |> optional "picture" (nullable string) Nothing
        |> required "passengers" int
        |> required "location" string
        |> optional "onCampus" (nullable bool) Nothing
        |> optional "about" (nullable string) Nothing
        |> optional "major" (nullable string) Nothing
        |> optional "minor" (nullable string) Nothing
        |> optional "hometown" (nullable string) Nothing
        |> optional "arrivedAtTech" (nullable int) Nothing
        |> optional "gatewayDrug" (nullable string) Nothing
        |> optional "conflicts" (nullable string) Nothing
        |> optional "dietaryRestrictions" (nullable string) Nothing
        |> optional "section" (nullable string) Nothing
        |> optional "enrollment" enrollmentDecoder Nothing
        |> optional "permissions" (Decode.list memberPermissionDecoder) []
        |> optional "positions" (Decode.list string) []
        |> optional "grades" (nullable gradesDecoder) Nothing


type alias MemberPermission =
    { name : String
    , eventType : Maybe String
    }


memberPermissionDecoder : Decoder MemberPermission
memberPermissionDecoder =
    Decode.succeed MemberPermission
        |> required "name" string
        |> optional "eventType" (nullable string) Nothing


type alias MemberRole =
    { member : Member
    , role : Role
    }


memberRoleDecoder : Decoder MemberRole
memberRoleDecoder =
    Decode.succeed MemberRole
        |> required "member" memberDecoder
        |> required "role" roleDecoder
