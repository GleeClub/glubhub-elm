module Models.Document exposing (Announcement, DocumentLink, MeetingMinutes, announcementDecoder, documentLinkDecoder, meetingMinutesDecoder)

import Json.Decode as Decode exposing (Decoder, bool, int, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Models.Info exposing (posixDecoder)
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
        |> optional "public" (nullable string) Nothing
        |> optional "private" (nullable string) Nothing


type alias DocumentLink =
    { name : String
    , url : String
    }


documentLinkDecoder =
    Decode.succeed DocumentLink
        |> required "name" string
        |> required "url" string


type alias Announcement =
    { id : Int
    , member : Maybe String
    , semester : String
    , time : Posix
    , content : String
    , archived : Bool
    }


announcementDecoder =
    Decode.succeed Announcement
        |> required "id" int
        |> optional "member" (nullable string) Nothing
        |> required "semester" string
        |> required "time" posixDecoder
        |> required "content" string
        |> required "archived" bool
