module Models.Admin exposing
    ( AbsenceRequest
    , AbsenceRequestState(..)
    , Fee
    , GigRequest
    , GigRequestStatus(..)
    , MemberPermission
    , RolePermission
    , absenceRequestDecoder
    , absenceRequestStateDecoder
    , feeDecoder
    , gigRequestDecoder
    , gigRequestStatusDecoder
    , memberPermissionDecoder
    , rolePermissionDecoder
    )

import Json.Decode as Decode exposing (Decoder, int, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Models.Info exposing (posixDecoder)
import Time exposing (Posix)


type alias MemberPermission =
    { name : String
    , eventType : Maybe String
    }


memberPermissionDecoder : Decoder MemberPermission
memberPermissionDecoder =
    Decode.succeed MemberPermission
        |> required "name" string
        |> optional "eventType" (nullable string) Nothing


type alias RolePermission =
    { role : String
    , permission : String
    , eventType : Maybe String
    }


rolePermissionDecoder : Decoder RolePermission
rolePermissionDecoder =
    Decode.succeed RolePermission
        |> required "role" string
        |> required "permission" string
        |> optional "eventType" (nullable string) Nothing


type alias Fee =
    { name : String
    , description : String
    , amount : Int
    }


feeDecoder : Decoder Fee
feeDecoder =
    Decode.succeed Fee
        |> required "name" string
        |> required "description" string
        |> required "amount" int


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
    = AbsenceRequestApproved
    | AbsenceRequestDenied
    | AbsenceRequestPending


absenceRequestStateDecoder : Decoder AbsenceRequestState
absenceRequestStateDecoder =
    string
        |> Decode.andThen
            (\state ->
                case state of
                    "Approved" ->
                        Decode.succeed AbsenceRequestApproved

                    "Denied" ->
                        Decode.succeed AbsenceRequestDenied

                    "Pending" ->
                        Decode.succeed AbsenceRequestPending

                    _ ->
                        Decode.fail "invalid absence request state"
            )


type alias GigRequest =
    { id : Int
    , time : Posix
    , name : String
    , organization : String
    , event : Maybe Int
    , contactName : String
    , contactEmail : String
    , contactPhone : String
    , startTime : Posix
    , location : String
    , comments : Maybe String
    , status : GigRequestStatus
    }


gigRequestDecoder : Decoder GigRequest
gigRequestDecoder =
    Decode.succeed GigRequest
        |> required "id" int
        |> required "time" posixDecoder
        |> required "name" string
        |> required "organization" string
        |> optional "event" (nullable int) Nothing
        |> required "contactName" string
        |> required "contactEmail" string
        |> required "contactPhone" string
        |> required "startTime" posixDecoder
        |> required "location" string
        |> optional "comments" (nullable string) Nothing
        |> required "status" gigRequestStatusDecoder


type GigRequestStatus
    = GigRequestAccepted
    | GigRequestDismissed
    | GigRequestPending


gigRequestStatusDecoder : Decoder GigRequestStatus
gigRequestStatusDecoder =
    string
        |> Decode.andThen
            (\state ->
                case state of
                    "Accepted" ->
                        Decode.succeed GigRequestAccepted

                    "Dismissed" ->
                        Decode.succeed GigRequestDismissed

                    "Pending" ->
                        Decode.succeed GigRequestPending

                    _ ->
                        Decode.fail "invalid gig request state"
            )
