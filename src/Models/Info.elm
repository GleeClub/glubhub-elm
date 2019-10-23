module Models.Info exposing (Enrollment(..), EventType, Info, MediaType, Permission, PermissionType(..), Role, Semester, StorageType(..), Uniform, enrollmentDecoder, eventTypeDecoder, infoDecoder, mediaTypeDecoder, permissionDecoder, permissionTypeDecoder, posixDecoder, roleDecoder, semesterDecoder, storageTypeDecoder, uniformDecoder)

import Json.Decode as Decode exposing (Decoder, bool, float, int, maybe, nullable, string)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Time exposing (Posix)


posixDecoder : Decoder Posix
posixDecoder =
    int |> Decode.andThen (\i -> Decode.succeed <| Time.millisToPosix i)


type alias Info =
    { eventTypes : List EventType
    , mediaTypes : List MediaType
    , permissions : List Permission
    , roles : List Role
    , sections : List String
    , transactionTypes : List String
    , uniforms : List Uniform
    }


infoDecoder : Decoder Info
infoDecoder =
    Decode.succeed Info
        |> required "eventTypes" (Decode.list eventTypeDecoder)
        |> required "mediaType" (Decode.list mediaTypeDecoder)
        |> required "permissions" (Decode.list permissionDecoder)
        |> required "roles" (Decode.list roleDecoder)
        |> required "sections" (Decode.list string)
        |> required "transactionTypes" (Decode.list string)
        |> required "uniforms" (Decode.list uniformDecoder)


type Enrollment
    = Class
    | Club


enrollmentDecoder : Decoder (Maybe Enrollment)
enrollmentDecoder =
    maybe string
        |> Decode.andThen
            (\x ->
                case x of
                    Just "inactive" ->
                        Decode.succeed Nothing

                    Just "" ->
                        Decode.succeed Nothing

                    Nothing ->
                        Decode.succeed Nothing

                    Just "class" ->
                        Decode.succeed <| Just Class

                    Just "club" ->
                        Decode.succeed <| Just Club

                    other ->
                        Decode.fail "Enrollments can only be \"class\", \"club\", \"inactive\", or null"
            )


type alias MediaType =
    { name : String
    , order : Int
    , storage : StorageType
    }


mediaTypeDecoder : Decoder MediaType
mediaTypeDecoder =
    Decode.succeed MediaType
        |> required "name" string
        |> required "order" int
        |> required "storage" storageTypeDecoder


type StorageType
    = LocalStorage
    | RemoteStorage


storageTypeDecoder : Decoder StorageType
storageTypeDecoder =
    string
        |> Decode.andThen
            (\x ->
                case x of
                    "local" ->
                        Decode.succeed LocalStorage

                    "remote" ->
                        Decode.succeed RemoteStorage

                    other ->
                        Decode.fail "StorageType can only be \"local\" or \"remote\""
            )


type alias EventType =
    { name : String
    , weight : Int
    }


eventTypeDecoder : Decoder EventType
eventTypeDecoder =
    Decode.succeed EventType
        |> required "name" string
        |> required "weight" int


type alias Permission =
    { name : String
    , description : Maybe String
    , type_ : PermissionType
    }


permissionDecoder : Decoder Permission
permissionDecoder =
    Decode.succeed Permission
        |> required "name" string
        |> optional "description" (nullable string) Nothing
        |> required "type" permissionTypeDecoder


type PermissionType
    = StaticPermission
    | EventPermission


permissionTypeDecoder : Decoder PermissionType
permissionTypeDecoder =
    string
        |> Decode.andThen
            (\x ->
                case x of
                    "static" ->
                        Decode.succeed StaticPermission

                    "event" ->
                        Decode.succeed EventPermission

                    other ->
                        Decode.fail "PermissionType can only be \"static\" or \"event\""
            )


type alias Role =
    { name : String
    , rank : Int
    , maxQuantity : Int
    }


roleDecoder : Decoder Role
roleDecoder =
    Decode.succeed Role
        |> required "name" string
        |> required "rank" int
        |> required "maxQuantity" int


type alias Uniform =
    { id : Int
    , name : String
    , color : Maybe String
    , description : Maybe String
    }


uniformDecoder : Decoder Uniform
uniformDecoder =
    Decode.succeed Uniform
        |> required "id" int
        |> required "name" string
        |> optional "color" (nullable string) Nothing
        |> optional "description" (nullable string) Nothing


type alias Semester =
    { name : String
    , startDate : Posix
    , endDate : Posix
    , gigRequirement : Int
    , current : Bool
    }


semesterDecoder : Decoder Semester
semesterDecoder =
    Decode.succeed Semester
        |> required "name" string
        |> required "startDate" posixDecoder
        |> required "endDate" posixDecoder
        |> required "gigRequirement" int
        |> required "current" bool
