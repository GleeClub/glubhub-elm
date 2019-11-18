port module Utils exposing (Common, RemoteData(..), alert, apiUrl, dateFormatter, eventIsOver, formatPhone, fullDateTimeFormatter, getRequest, handleJsonResponse, notFoundView, permittedTo, postRequest, rawHtml, romanNumeral, scrollToElement, setToken, simpleDateFormatter, simpleDateTimeFormatter, spinner, timeFormatter, timeFromNow)

import Browser.Navigation as Nav
import DateFormat
import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode as Decode exposing (Decoder)
import Models.Event exposing (FullEvent, Member)
import Models.Info exposing (Info, Semester)
import Time exposing (Posix, Zone, millisToPosix, now, posixToMillis, toMonth)



---- CONSTANTS ----


apiUrl =
    "https://gleeclub.gatech.edu/cgi-bin/api"



---- REUSED COMPONENTS ----


spinner : Html a
spinner =
    div [ class "spinner" ]
        [ div [ class "spinner-inner" ]
            [ i [ class "oldgold-text fas fa-circle-notch fa-2x fa-spin" ] []
            ]
        ]


notFoundView : Html a
notFoundView =
    div []
        [ text "Not found"
        ]


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
    | Failure


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



---- FUNCTIONS ----


getRequest : Common -> String -> Http.Expect msg -> Cmd msg
getRequest common url expect =
    Http.request
        { method = "GET"
        , url = apiUrl ++ url
        , body = Http.emptyBody
        , headers = [ Http.header "token" common.token ]
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


postRequest : Common -> String -> Http.Body -> Http.Expect msg -> Cmd msg
postRequest common url body expect =
    Http.request
        { method = "POST"
        , url = apiUrl ++ url
        , body = body
        , headers = [ Http.header "token" common.token ]
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


fullDateTimeFormatter : Zone -> Posix -> String
fullDateTimeFormatter zone dateTime =
    let
        formattingOptions =
            [ DateFormat.dayOfWeekNameFull
            , DateFormat.text ", "
            , DateFormat.monthNameFull
            , DateFormat.text " "
            , DateFormat.dayOfMonthNumber
            , DateFormat.text ", "
            , DateFormat.yearNumber
            , DateFormat.text " "
            , DateFormat.hourNumber
            , DateFormat.text ":"
            , DateFormat.minuteFixed
            , DateFormat.text " "
            , DateFormat.amPmUppercase
            ]
    in
    DateFormat.format formattingOptions zone dateTime


simpleDateTimeFormatter : Zone -> Posix -> String
simpleDateTimeFormatter zone dateTime =
    let
        formattingOptions =
            [ DateFormat.monthNameAbbreviated
            , DateFormat.text " "
            , DateFormat.dayOfMonthNumber
            , DateFormat.text " "
            , DateFormat.hourNumber
            , DateFormat.text ":"
            , DateFormat.minuteFixed
            , DateFormat.text " "
            , DateFormat.amPmUppercase
            ]
    in
    DateFormat.format formattingOptions zone dateTime


timeFormatter : Zone -> Posix -> String
timeFormatter zone dateTime =
    let
        formattingOptions =
            [ DateFormat.hourNumber
            , DateFormat.text ":"
            , DateFormat.minuteFixed
            , DateFormat.text " "
            , DateFormat.amPmUppercase
            ]
    in
    DateFormat.format formattingOptions zone dateTime


dateFormatter : Zone -> Posix -> String
dateFormatter zone dateTime =
    let
        formattingOptions =
            [ DateFormat.dayOfWeekNameFull
            , DateFormat.text ", "
            , DateFormat.monthNameFull
            , DateFormat.text " "
            , DateFormat.dayOfMonthNumber
            ]
    in
    DateFormat.format formattingOptions zone dateTime


simpleDateFormatter : Zone -> Posix -> String
simpleDateFormatter zone dateTime =
    let
        formattingOptions =
            [ DateFormat.monthNumber
            , DateFormat.text "/"
            , DateFormat.dayOfMonthNumber
            ]
    in
    DateFormat.format formattingOptions zone dateTime


timeFromNow : Common -> Posix -> String
timeFromNow common then_ =
    let
        nowMillis =
            posixToMillis common.now

        thenMillis =
            posixToMillis then_

        diffMinutes =
            (thenMillis - nowMillis) * 1000 // 60
    in
    if diffMinutes < 60 then
        "in " ++ String.fromInt diffMinutes ++ " minutes"

    else if diffMinutes < 60 * 24 then
        "in " ++ String.fromInt (diffMinutes // 60) ++ " hours"

    else
        "in " ++ String.fromInt (diffMinutes // (60 * 24)) ++ " days"


eventIsOver : Posix -> FullEvent -> Bool
eventIsOver now event =
    posixToMillis now < posixToMillis (event.releaseTime |> Maybe.withDefault event.callTime)


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


permittedTo : String -> Member -> Bool
permittedTo permission user =
    user.permissions |> List.any (\p -> p.name == permission)


port setToken : Maybe String -> Cmd msg


port alert : String -> Cmd msg


port scrollToElement : String -> Cmd msg
