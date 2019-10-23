module Models.Song exposing (Pitch(..), Song, SongLink, SongLinkSection, SongMode(..), pitchDecoder, pitchToString, songDecoder, songLinkDecoder, songLinkSectionDecoder, songModeDecoder)

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


type Pitch
    = AFlat
    | A
    | ASharp
    | BFlat
    | B
    | BSharp
    | CFlat
    | C
    | CSharp
    | DFlat
    | D
    | DSharp
    | EFlat
    | E
    | ESharp
    | FFlat
    | F
    | FSharp
    | GFlat
    | G
    | GSharp


pitchDecoder : Decoder Pitch
pitchDecoder =
    string
        |> Decode.andThen
            (\pitch ->
                case pitch of
                    "A♭" ->
                        Decode.succeed AFlat

                    "A" ->
                        Decode.succeed A

                    "A♯" ->
                        Decode.succeed ASharp

                    "B♭" ->
                        Decode.succeed BFlat

                    "B" ->
                        Decode.succeed B

                    "B♯" ->
                        Decode.succeed BSharp

                    "C♭" ->
                        Decode.succeed CFlat

                    "C" ->
                        Decode.succeed C

                    "C♯" ->
                        Decode.succeed CSharp

                    "D♭" ->
                        Decode.succeed DFlat

                    "D" ->
                        Decode.succeed D

                    "D♯" ->
                        Decode.succeed DSharp

                    "E♭" ->
                        Decode.succeed EFlat

                    "E" ->
                        Decode.succeed E

                    "E♯" ->
                        Decode.succeed ESharp

                    "F♭" ->
                        Decode.succeed FFlat

                    "F" ->
                        Decode.succeed F

                    "F♯" ->
                        Decode.succeed FSharp

                    "G♭" ->
                        Decode.succeed GFlat

                    "G" ->
                        Decode.succeed G

                    "G♯" ->
                        Decode.succeed GSharp

                    other ->
                        Decode.fail "SongMode can only be valid keys"
            )


pitchToString : Pitch -> String
pitchToString pitch =
    case pitch of
        AFlat ->
            "A♭"

        A ->
            "A"

        ASharp ->
            "A♯"

        BFlat ->
            "B♭"

        B ->
            "B"

        BSharp ->
            "B♯"

        CFlat ->
            "C♭"

        C ->
            "C"

        CSharp ->
            "C♯"

        DFlat ->
            "D♭"

        D ->
            "D"

        DSharp ->
            "D♯"

        EFlat ->
            "E♭"

        E ->
            "E"

        ESharp ->
            "E♯"

        FFlat ->
            "F♭"

        F ->
            "F"

        FSharp ->
            "F♯"

        GFlat ->
            "G♭"

        G ->
            "G"

        GSharp ->
            "G♯"
