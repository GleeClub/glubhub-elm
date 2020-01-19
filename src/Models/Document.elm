module Models.Document exposing (Announcement, MeetingMinutes, announcementDecoder, meetingMinutesDecoder)

import Json.Decode as Decode exposing (Decoder, bool, int, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Models.Info exposing (optionalStringDecoder, posixDecoder)
import Time exposing (Posix)


type alias MeetingMinutes =
    { id : Int
    , name : String
    , date : Posix
    , public : Maybe String
    , private : Maybe String
    }


meetingMinutesDecoder : Decoder MeetingMinutes
meetingMinutesDecoder =
    Decode.succeed MeetingMinutes
        |> required "id" int
        |> required "name" string
        |> required "date" posixDecoder
        |> optional "public" optionalStringDecoder Nothing
        |> optional "private" optionalStringDecoder Nothing


type alias Announcement =
    { id : Int
    , member : Maybe String
    , semester : String
    , time : Posix
    , content : String
    , archived : Bool
    }


announcementDecoder : Decoder Announcement
announcementDecoder =
    Decode.succeed Announcement
        |> required "id" int
        |> optional "member" (nullable string) Nothing
        |> required "semester" string
        |> required "time" posixDecoder
        |> required "content" string
        |> required "archived" bool
