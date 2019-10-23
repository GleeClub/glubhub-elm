module Models.Member exposing (Member, memberDecoder)

import Json.Decode as Decode exposing (Decoder, bool, float, int, maybe, nullable, string)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Models.Event exposing (Grades, gradesDecoder)
import Models.Info exposing (Enrollment, enrollmentDecoder, posixDecoder)
import Time exposing (Posix)


type alias Member =
    { email : String
    , firstName : String
    , preferredName : Maybe String
    , lastName : String
    , fullName : String
    , phoneNumber : String
    , picture : Maybe String
    , passengers : Int
    , location : String
    , onCampus : Maybe Bool
    , about : Maybe String
    , major : Maybe String
    , minor : Maybe String
    , hometown : Maybe String
    , arrivedAtTech : Maybe Int
    , gatewayDrug : Maybe String
    , conflicts : Maybe String
    , dietaryRestrictions : Maybe String
    , section : Maybe String
    , enrollment : Maybe Enrollment
    , permissions : List String
    , positions : List String
    , grades : Maybe Grades
    }


memberDecoder : Decoder Member
memberDecoder =
    Decode.succeed Member
        |> required "email" string
        |> required "firstName" string
        |> optional "preferredName" (nullable string) Nothing
        |> required "lastName" string
        |> required "fullName" string
        |> required "phoneNumber" string
        |> optional "picture" (nullable string) Nothing
        |> required "passengers" int
        |> required "location" string
        |> optional "onCampus" (nullable bool) Nothing
        |> optional "about" (nullable string) Nothing
        |> optional "major" (nullable string) Nothing
        |> optional "minor" (nullable string) Nothing
        |> optional "hometown" (nullable string) Nothing
        |> optional "arrivedAtTech" (nullable int) Nothing
        |> optional "gatewayDrug" (nullable string) Nothing
        |> optional "conflicts" (nullable string) Nothing
        |> optional "dietaryRestrictions" (nullable string) Nothing
        |> optional "section" (nullable string) Nothing
        |> optional "enrollment" enrollmentDecoder Nothing
        |> optional "permissions" (Decode.list string) []
        |> optional "positions" (Decode.list string) []
        |> optional "grades" (maybe gradesDecoder) Nothing
