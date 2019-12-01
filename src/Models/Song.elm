module Models.Song exposing (Accidental(..), BasePitch(..), Pitch, Song, SongLink, SongLinkSection, SongMode(..), halfStepsAboveA, pitchDecoder, pitchToString, songDecoder, songLinkDecoder, songLinkSectionDecoder, songModeDecoder, songModeToString)

import Json.Decode as Decode exposing (Decoder, bool, float, int, maybe, nullable, string)
import Json.Decode.Pipeline exposing (custom, optional, required)


type alias Song =
    { id : Int
    , title : String
    , info : Maybe String
    , current : Bool
    , key : Maybe Pitch
    , startingPitch : Maybe Pitch
    , mode : Maybe SongMode
    , links : Maybe (List SongLinkSection)
    }


songDecoder : Decoder Song
songDecoder =
    Decode.succeed Song
        |> required "id" int
        |> required "title" string
        |> optional "info" (nullable string) Nothing
        |> required "current" bool
        |> optional "key" (nullable pitchDecoder) Nothing
        |> optional "startingPitch" (nullable pitchDecoder) Nothing
        |> optional "mode" (nullable songModeDecoder) Nothing
        |> optional "links" (nullable <| Decode.list songLinkSectionDecoder) Nothing


type alias SongLinkSection =
    { name : String
    , links : List SongLink
    }


songLinkSectionDecoder : Decoder SongLinkSection
songLinkSectionDecoder =
    Decode.succeed SongLinkSection
        |> required "name" string
        |> required "links" (Decode.list songLinkDecoder)


type alias SongLink =
    { id : Int
    , song : Int
    , type_ : String
    , name : String
    , target : String
    }


songLinkDecoder : Decoder SongLink
songLinkDecoder =
    Decode.succeed SongLink
        |> required "id" int
        |> required "song" int
        |> required "type" string
        |> required "name" string
        |> required "target" string


type SongMode
    = Major
    | Minor


songModeToString : SongMode -> String
songModeToString mode =
    case mode of
        Major ->
            "major"

        Minor ->
            "minor"


songModeDecoder : Decoder SongMode
songModeDecoder =
    string
        |> Decode.andThen
            (\mode ->
                case mode of
                    "major" ->
                        Decode.succeed Major

                    "minor" ->
                        Decode.succeed Minor

                    other ->
                        Decode.fail "SongMode can only be \"major\" or \"minor\""
            )


type BasePitch
    = A
    | B
    | C
    | D
    | E
    | F
    | G


type Accidental
    = Natural
    | Flat
    | Sharp


type alias Pitch =
    { base : BasePitch
    , accidental : Accidental
    }


pitchDecoder : Decoder Pitch
pitchDecoder =
    string
        |> Decode.andThen
            (\pitch ->
                let
                    base =
                        case pitch |> String.slice 0 1 of
                            "A" ->
                                Decode.succeed A

                            "B" ->
                                Decode.succeed B

                            "C" ->
                                Decode.succeed C

                            "D" ->
                                Decode.succeed D

                            "E" ->
                                Decode.succeed E

                            "F" ->
                                Decode.succeed F

                            "G" ->
                                Decode.succeed G

                            _ ->
                                Decode.fail "invalid pitch"

                    accidental =
                        case pitch |> String.slice 1 2 of
                            "" ->
                                Decode.succeed Natural

                            "♭" ->
                                Decode.succeed Flat

                            "♯" ->
                                Decode.succeed Sharp

                            _ ->
                                Decode.fail "invalid accidental"
                in
                Decode.map2 Pitch base accidental
            )


pitchToString : Pitch -> String
pitchToString pitch =
    let
        base =
            case pitch.base of
                A ->
                    "A"

                B ->
                    "B"

                C ->
                    "C"

                D ->
                    "D"

                E ->
                    "E"

                F ->
                    "F"

                G ->
                    "G"

        accidental =
            case pitch.accidental of
                Natural ->
                    ""

                Flat ->
                    "♭"

                Sharp ->
                    "♯"
    in
    base ++ accidental


halfStepsAboveA : Pitch -> Int
halfStepsAboveA pitch =
    let
        base =
            case pitch.base of
                A ->
                    0

                B ->
                    2

                C ->
                    3

                D ->
                    5

                E ->
                    7

                F ->
                    8

                G ->
                    10

        accidental =
            case pitch.accidental of
                Natural ->
                    0

                Flat ->
                    -1

                Sharp ->
                    1
    in
    (base + accidental) |> modBy 12
