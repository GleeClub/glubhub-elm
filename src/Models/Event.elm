module Models.Event exposing
    ( ActiveSemester
    , Attendance
    , Event
    , EventAttendance
    , EventAttendee
    , EventCarpool
    , Gig
    , GradeChange
    , Grades
    , Member
    , MemberPermission
    , MemberRole
    , SimpleAttendance
    , SimpleGradeChange
    , UpdatedCarpool
    , activeSemesterDecoder
    , attendanceDecoder
    , defaultSimpleAttendance
    , eventAttendanceDecoder
    , eventAttendeeDecoder
    , eventCarpoolDecoder
    , eventDecoder
    , gigDecoder
    , gradeChangeDecoder
    , gradesDecoder
    , memberDecoder
    , memberPermissionDecoder
    , memberRoleDecoder
    , simpleAttendanceDecoder
    , simpleGradeChangeDecoder
    )

import Json.Decode as Decode exposing (Decoder, bool, float, int, nullable, string)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Models.Admin exposing (AbsenceRequest, absenceRequestDecoder)
import Models.Info
    exposing
        ( Enrollment
        , Role
        , enrollmentDecoder
        , optionalStringDecoder
        , posixDecoder
        , roleDecoder
        )
import Time exposing (Posix)


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
    , rsvpIssue : Maybe String
    , attendance : Maybe SimpleAttendance
    , gradeChange : Maybe SimpleGradeChange
    , absenceRequest : Maybe AbsenceRequest
    }


eventDecoder : Decoder Event
eventDecoder =
    let
        gigChecker json =
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
    in
    Decode.succeed Event
        |> required "id" int
        |> required "name" string
        |> required "semester" string
        |> required "type" string
        |> required "callTime" posixDecoder
        |> optional "releaseTime" (nullable posixDecoder) Nothing
        |> required "points" int
        |> optional "comments" optionalStringDecoder Nothing
        |> optional "location" optionalStringDecoder Nothing
        |> required "gigCount" bool
        |> required "defaultAttend" bool
        |> optional "section" (nullable string) Nothing
        |> custom (Decode.value |> Decode.andThen gigChecker)
        |> optional "rsvpIssue" (nullable string) Nothing
        |> optional "attendance" (nullable simpleAttendanceDecoder) Nothing
        |> optional "change" (nullable simpleGradeChangeDecoder) Nothing
        |> optional "absenceRequest" (nullable absenceRequestDecoder) Nothing


type alias SimpleGradeChange =
    { change : Float
    , reason : String
    , partialScore : Float
    }


simpleGradeChangeDecoder : Decoder SimpleGradeChange
simpleGradeChangeDecoder =
    Decode.succeed SimpleGradeChange
        |> required "change" float
        |> required "reason" string
        |> required "partialScore" float


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
        |> optional "contactName" optionalStringDecoder Nothing
        |> optional "contactEmail" optionalStringDecoder Nothing
        |> optional "contactPhone" optionalStringDecoder Nothing
        |> optional "price" (nullable int) Nothing
        |> required "public" bool
        |> optional "summary" optionalStringDecoder Nothing
        |> optional "description" optionalStringDecoder Nothing


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


type alias EventAttendance =
    { event : Event
    , attendance : SimpleAttendance
    }


eventAttendanceDecoder : Decoder EventAttendance
eventAttendanceDecoder =
    Decode.succeed EventAttendance
        |> required "event" eventDecoder
        |> required "attendance" simpleAttendanceDecoder


type alias EventAttendee =
    { member : Member
    , attendance : SimpleAttendance
    }


eventAttendeeDecoder : Decoder EventAttendee
eventAttendeeDecoder =
    Decode.succeed EventAttendee
        |> required "member" memberDecoder
        |> required "attendance" simpleAttendanceDecoder


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


defaultSimpleAttendance : SimpleAttendance
defaultSimpleAttendance =
    { shouldAttend = False
    , didAttend = False
    , confirmed = False
    , minutesLate = 0
    }


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
    { grade : Float
    , eventsWithChanges : List Event
    , volunteerGigsAttended : Int
    }


gradesDecoder : Decoder Grades
gradesDecoder =
    Decode.succeed Grades
        |> required "grade" float
        |> required "eventsWithChanges" (Decode.list eventDecoder)
        |> required "volunteerGigsAttended" int


type alias GradeChange =
    { event : Event
    , attendance : SimpleAttendance
    , reason : String
    , change : Float
    , partialScore : Float
    }


gradeChangeDecoder : Decoder GradeChange
gradeChangeDecoder =
    Decode.succeed GradeChange
        |> required "event" eventDecoder
        |> required "attendance" simpleAttendanceDecoder
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
    }


memberDecoder : Decoder Member
memberDecoder =
    Decode.succeed Member
        |> required "email" string
        |> required "firstName" string
        |> optional "preferredName" optionalStringDecoder Nothing
        |> required "lastName" string
        |> required "phoneNumber" string
        |> optional "picture" optionalStringDecoder Nothing
        |> required "passengers" int
        |> required "location" string
        |> optional "onCampus" (nullable bool) Nothing
        |> optional "about" optionalStringDecoder Nothing
        |> optional "major" optionalStringDecoder Nothing
        |> optional "minor" optionalStringDecoder Nothing
        |> optional "hometown" optionalStringDecoder Nothing
        |> optional "arrivedAtTech" (nullable int) Nothing
        |> optional "gatewayDrug" optionalStringDecoder Nothing
        |> optional "conflicts" optionalStringDecoder Nothing
        |> optional "dietaryRestrictions" optionalStringDecoder Nothing
        |> optional "section" (nullable string) Nothing
        |> optional "enrollment" enrollmentDecoder Nothing
        |> optional "permissions" (Decode.list memberPermissionDecoder) []
        |> optional "positions" (Decode.list string) []


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


type alias ActiveSemester =
    { semester : String
    , enrollment : Maybe Enrollment
    , section : Maybe String
    , grades : Grades
    }


activeSemesterDecoder : Decoder ActiveSemester
activeSemesterDecoder =
    Decode.succeed ActiveSemester
        |> required "semester" string
        |> required "enrollment" enrollmentDecoder
        |> optional "section" (nullable string) Nothing
        |> required "grades" gradesDecoder
