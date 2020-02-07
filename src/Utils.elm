port module Utils exposing
    ( Common
    , RemoteData(..)
    , SubmissionState(..)
    , alert
    , checkSubmissionResult
    , deployEditor
    , eventIsOver
    , formatPhone
    , fullName
    , getMemberName
    , goldColor
    , isActiveClass
    , isDisabledClass
    , isLoadingClass
    , isPrimaryClass
    , mapLoaded
    , optionalSingleton
    , permittedTo
    , playPitch
    , rawHtml
    , remoteToMaybe
    , resultToRemote
    , resultToSubmissionState
    , romanNumeral
    , roundToTwoDigits
    , scrollToElement
    , setOldToken
    , setToken
    , submissionStateBoxId
    )

import Browser.Navigation as Nav
import Color exposing (Color)
import Error exposing (GreaseError)
import Html exposing (Html, i, text)
import Html.Parser
import Html.Parser.Util
import List.Extra exposing (find)
import Models.Event exposing (Event, Member)
import Models.Info exposing (Info, Semester)
import Time exposing (Posix, Zone, posixToMillis)



---- CONSTANTS ----


goldColor : Color
goldColor =
    Color.rgb 0.706 0.643 0.412


submissionStateBoxId : String
submissionStateBoxId =
    "submission-state-box"



---- REUSED COMPONENTS ----


rawHtml : String -> List (Html msg)
rawHtml content =
    case Html.Parser.run content of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []



---- TYPES ----


type RemoteData a
    = NotAsked
    | Loading
    | Loaded a
    | Failure GreaseError


type alias Common =
    { user : Maybe Member
    , members : List Member
    , info : Info
    , currentSemester : Semester
    , token : String
    , key : Nav.Key
    , timeZone : Zone
    , now : Posix
    }


type SubmissionState
    = NotSentYet
    | Sending
    | ErrorSending GreaseError



---- FUNCTIONS ----


mapLoaded : (a -> b) -> RemoteData a -> RemoteData b
mapLoaded mapper remoteData =
    case remoteData of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Loaded data ->
            Loaded (mapper data)

        Failure err ->
            Failure err


remoteToMaybe : RemoteData a -> Maybe a
remoteToMaybe remoteData =
    case remoteData of
        Loaded data ->
            Just data

        _ ->
            Nothing


resultToRemote : Result GreaseError a -> RemoteData a
resultToRemote result =
    case result of
        Ok success ->
            Loaded success

        Err error ->
            Failure error


resultToSubmissionState : Result GreaseError a -> SubmissionState
resultToSubmissionState result =
    case result of
        Ok _ ->
            NotSentYet

        Err error ->
            ErrorSending error


checkSubmissionResult :
    { a | state : SubmissionState }
    -> Result GreaseError ()
    -> ( { a | state : SubmissionState }, Cmd msg )
checkSubmissionResult model result =
    case result of
        Ok _ ->
            ( { model | state = NotSentYet }, Cmd.none )

        Err error ->
            ( { model | state = ErrorSending error }
            , scrollToElement submissionStateBoxId
            )


eventIsOver : Common -> Event -> Bool
eventIsOver { now } { callTime, releaseTime } =
    posixToMillis now > posixToMillis (releaseTime |> Maybe.withDefault callTime)


formatPhone : String -> String
formatPhone phone =
    if String.length phone == 10 then
        "(" ++ String.slice 0 3 phone ++ ") " ++ String.slice 3 6 phone ++ "-" ++ String.slice 6 10 phone

    else
        phone


romanNumeral : Int -> String
romanNumeral n =
    let
        numbers =
            [ ( "zero", "0" )
            , ( "one", "I" )
            , ( "two", "II" )
            , ( "three", "III" )
            , ( "four", "IV" )
            , ( "five", "V" )
            , ( "six", "VI" )
            , ( "seven", "VII" )
            , ( "eight", "VIII" )
            , ( "nine", "IX" )
            , ( "ten", "X" )
            , ( "eleven", "XI" )
            , ( "twelve", "XII" )
            , ( "thirteen", "XIII" )
            , ( "fourteen", "XIV" )
            , ( "fifteen", "XV" )
            , ( "sixteen", "XVI" )
            , ( "seventeen", "XVII" )
            , ( "eighteen", "XVIII" )
            , ( "nineteen", "XIX" )
            , ( "twenty", "XX" )
            ]
    in
    case numbers |> List.drop n |> List.head of
        Just ( number, numeral ) ->
            number ++ " (" ++ numeral ++ ")"

        Nothing ->
            String.fromInt n


roundToTwoDigits : Float -> Float
roundToTwoDigits x =
    toFloat (round (x * 100.0)) / 100.0


permittedTo : String -> Member -> Bool
permittedTo permission user =
    user.permissions |> List.any (\p -> p.name == permission)


fullName : Member -> String
fullName member =
    let
        firstName =
            member.preferredName
                |> Maybe.withDefault member.firstName
    in
    firstName ++ " " ++ member.lastName


getMemberName : Common -> String -> Html msg
getMemberName common email =
    common.members
        |> find (\member -> member.email == email)
        |> Maybe.map (fullName >> text)
        |> Maybe.withDefault (i [] [ text email ])


isLoadingClass : Bool -> String
isLoadingClass isLoading =
    if isLoading then
        " is-loading"

    else
        ""


isDisabledClass : Bool -> String
isDisabledClass isDisabled =
    if isDisabled then
        " is-disabled"

    else
        ""


isPrimaryClass : Bool -> String
isPrimaryClass isPrimary =
    if isPrimary then
        " is-primary"

    else
        ""


isActiveClass : Bool -> String
isActiveClass isActive =
    if isActive then
        " is-active"

    else
        ""


optionalSingleton : Bool -> Html msg -> List (Html msg)
optionalSingleton shouldRender content =
    if shouldRender then
        [ content ]

    else
        []



-- isLoadingClass : Bool -> String
-- isLoadingClass isLoading =
--     if isLoading then
--         " is-loading"
--     else
--         ""
-- PORTS --


port setToken : Maybe String -> Cmd msg


port setOldToken : Maybe String -> Cmd msg


port alert : String -> Cmd msg


port scrollToElement : String -> Cmd msg


port playPitch : Int -> Cmd msg


port deployEditor : { elementId : String, content : String } -> Cmd msg
