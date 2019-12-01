module Models.Admin exposing (AbsenceRequest, AbsenceRequestState(..), Fee, MemberPermission, RolePermission, absenceRequestDecoder, absenceRequestStateDecoder, feeDecoder, memberPermissionDecoder, rolePermissionDecoder)

import Json.Decode as Decode exposing (Decoder, bool, int, nullable, string)
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
    = Approved
    | Denied
    | Pending


absenceRequestStateDecoder : Decoder AbsenceRequestState
absenceRequestStateDecoder =
    string
        |> Decode.andThen
            (\state ->
                case state of
                    "approved" ->
                        Decode.succeed Approved

                    "denied" ->
                        Decode.succeed Denied

                    "pending" ->
                        Decode.succeed Pending

                    _ ->
                        Decode.fail "invalid absence request state"
            )
