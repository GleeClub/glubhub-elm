module Models.Document exposing (MeetingMinutes, meetingMinutesDecoder)

import Json.Decode as Decode exposing (Decoder, int, nullable, string)
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
