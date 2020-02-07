module Models.Song exposing
    ( Accidental(..)
    , BasePitch(..)
    , Pitch
    , Song
    , SongLink
    , SongLinkSection
    , SongMode(..)
    , allAccidentals
    , allBasePitches
    , allPitches
    , halfStepsAboveA
    , pitchDecoder
    , pitchEncoder
    , pitchFromUnicode
    , pitchToUnicode
    , songDecoder
    , songLinkDecoder
    , songLinkSectionDecoder
    , songModeDecoder
    , songModeEncoder
    , songModeFromString
    , songModeToString
    )

import Json.Decode as Decode exposing (Decoder, bool, int, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Song =
    { id : Int
    , title : String
    , info : Maybe String
    , current : Bool
    , key : Maybe Pitch
    , startingPitch : Maybe Pitch
    , mode : Maybe SongMode
    , links : List SongLinkSection
    }


songDecoder : Decoder Song
songDecoder =
    Decode.succeed Song
        |> required "id" int
        |> required "title" string
        |> optional "info" (nullable string) Nothing
        |> required "current" bool
        |> optional "key" pitchDecoder Nothing
        |> optional "startingPitch" pitchDecoder Nothing
        |> optional "mode" (nullable songModeDecoder) Nothing
        |> optional "links" (Decode.list songLinkSectionDecoder) []


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
            "Major"

        Minor ->
            "Minor"


songModeFromString : String -> Maybe SongMode
songModeFromString mode =
    case mode of
        "Major" ->
            Just Major

        "Minor" ->
            Just Minor

        _ ->
            Nothing


songModeDecoder : Decoder SongMode
songModeDecoder =
    string
        |> Decode.andThen
            (\mode ->
                case mode of
                    "Major" ->
                        Decode.succeed Major

                    "Minor" ->
                        Decode.succeed Minor

                    _ ->
                        Decode.fail "SongMode can only be \"Major\" or \"Minor\""
            )


songModeEncoder : SongMode -> Encode.Value
songModeEncoder songMode =
    case songMode of
        Major ->
            Encode.string "Major"

        Minor ->
            Encode.string "Minor"


type BasePitch
    = A
    | B
    | C
    | D
    | E
    | F
    | G


allBasePitches : List BasePitch
allBasePitches =
    [ A, B, C, D, E, F, G ]


type Accidental
    = Natural
    | Flat
    | Sharp


allAccidentals : List Accidental
allAccidentals =
    [ Natural, Flat, Sharp ]


type alias Pitch =
    { base : BasePitch
    , accidental : Accidental
    }


allPitches : List Pitch
allPitches =
    let
        allPitchVariations basePitch =
            allAccidentals
                |> List.map
                    (\accidental ->
                        { base = basePitch
                        , accidental = accidental
                        }
                    )
    in
    allBasePitches |> List.concatMap allPitchVariations


pitchDecoder : Decoder (Maybe Pitch)
pitchDecoder =
    nullable string
        |> Decode.map (Maybe.withDefault "")
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
                        case pitch |> String.dropLeft 1 of
                            "" ->
                                Decode.succeed Natural

                            "Flat" ->
                                Decode.succeed Flat

                            "Sharp" ->
                                Decode.succeed Sharp

                            _ ->
                                Decode.fail "invalid accidental"
                in
                if String.isEmpty pitch then
                    Decode.succeed Nothing

                else
                    Decode.map2 (\b a -> Just <| Pitch b a) base accidental
            )


pitchEncoder : Pitch -> Encode.Value
pitchEncoder pitch =
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
                    "Flat"

                Sharp ->
                    "Sharp"
    in
    Encode.string <| base ++ accidental


pitchToUnicode : Pitch -> String
pitchToUnicode pitch =
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


pitchFromUnicode : String -> Maybe Pitch
pitchFromUnicode pitch =
    let
        base =
            case pitch |> String.slice 0 1 of
                "A" ->
                    Just A

                "B" ->
                    Just B

                "C" ->
                    Just C

                "D" ->
                    Just D

                "E" ->
                    Just E

                "F" ->
                    Just F

                "G" ->
                    Just G

                _ ->
                    Nothing

        accidental =
            case pitch |> String.dropLeft 1 of
                "" ->
                    Just Natural

                "Flat" ->
                    Just Flat

                "Sharp" ->
                    Just Sharp

                _ ->
                    Nothing
    in
    Maybe.map2 Pitch base accidental


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
