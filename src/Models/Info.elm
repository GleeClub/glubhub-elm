module Models.Info exposing
    ( DocumentLink
    , Enrollment(..)
    , EventType
    , Info
    , MediaType
    , Permission
    , PermissionType(..)
    , Role
    , Semester
    , StorageType(..)
    , Uniform
    , documentLinkDecoder
    , enrollmentDecoder
    , enrollmentToString
    , eventTypeDecoder
    , infoDecoder
    , mediaTypeDecoder
    , permissionDecoder
    , permissionTypeDecoder
    , posixDecoder
    , roleDecoder
    , semesterDecoder
    , storageTypeDecoder
    , uniformDecoder
    )

import Json.Decode as Decode exposing (Decoder, bool, int, maybe, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Time exposing (Posix, millisToPosix)


posixDecoder : Decoder Posix
posixDecoder =
    int |> Decode.map Time.millisToPosix


type alias Info =
    { eventTypes : List EventType
    , mediaTypes : List MediaType
    , permissions : List Permission
    , roles : List Role
    , sections : List String
    , transactionTypes : List String
    , uniforms : List Uniform
    , documents : List DocumentLink
    }


infoDecoder : Decoder Info
infoDecoder =
    Decode.succeed Info
        |> required "eventTypes" (Decode.list eventTypeDecoder)
        |> required "mediaTypes" (Decode.list mediaTypeDecoder)
        |> required "permissions" (Decode.list permissionDecoder)
        |> required "roles" (Decode.list roleDecoder)
        |> required "sections" (Decode.list string)
        |> required "transactionTypes" (Decode.list string)
        |> required "uniforms" (Decode.list uniformDecoder)
        |> optional "documents" (Decode.list documentLinkDecoder) []


type alias DocumentLink =
    { name : String
    , url : String
    }


documentLinkDecoder : Decoder DocumentLink
documentLinkDecoder =
    Decode.succeed DocumentLink
        |> required "name" string
        |> required "url" string


type Enrollment
    = Class
    | Club


enrollmentDecoder : Decoder (Maybe Enrollment)
enrollmentDecoder =
    maybe string
        |> Decode.andThen
            (\x ->
                case x of
                    Nothing ->
                        Decode.succeed Nothing

                    Just "Class" ->
                        Decode.succeed <| Just Class

                    Just "Club" ->
                        Decode.succeed <| Just Club

                    _ ->
                        Decode.fail "Enrollments can only be \"Class\", \"Club\", or null"
            )


enrollmentToString : Enrollment -> String
enrollmentToString enrollment =
    case enrollment of
        Class ->
            "Class"

        Club ->
            "Club"


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
                    "Local" ->
                        Decode.succeed LocalStorage

                    "Remote" ->
                        Decode.succeed RemoteStorage

                    _ ->
                        Decode.fail "StorageType can only be \"Local\" or \"Remote\""
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
                    "Static" ->
                        Decode.succeed StaticPermission

                    "Event" ->
                        Decode.succeed EventPermission

                    _ ->
                        Decode.fail "PermissionType can only be \"Static\" or \"Event\""
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
