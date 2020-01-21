module Datetime exposing (dateFormatter, fullDateTimeFormatter, hyphenDateFormatter, parseFormDateAndTimeString, parseFormDateString, simpleDateFormatter, simpleDateTimeFormatter, timeFormatter, timeFromNow, twentyFourHourTimeFormatter)

import DateFormat
import Time exposing (Month, Posix, Zone, posixToMillis)
import Time.Extra exposing (partsToPosix)
import Utils exposing (Common)


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


hyphenDateFormatter : Zone -> Posix -> String
hyphenDateFormatter zone dateTime =
    let
        formattingOptions =
            [ DateFormat.yearNumber
            , DateFormat.text "-"
            , DateFormat.monthFixed
            , DateFormat.text "-"
            , DateFormat.dayOfMonthFixed
            ]
    in
    DateFormat.format formattingOptions zone dateTime


twentyFourHourTimeFormatter : Zone -> Posix -> String
twentyFourHourTimeFormatter zone dateTime =
    let
        formattingOptions =
            [ DateFormat.hourMilitaryFixed
            , DateFormat.text ":"
            , DateFormat.minuteFixed
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
            (thenMillis - nowMillis) // (1000 * 60)
    in
    if diffMinutes < 60 then
        "in " ++ String.fromInt diffMinutes ++ " minutes"

    else if diffMinutes < 60 * 24 then
        "in " ++ String.fromInt (diffMinutes // 60) ++ " hours"

    else
        "in " ++ String.fromInt (diffMinutes // (60 * 24)) ++ " days"


monthFromInt : Int -> Maybe Month
monthFromInt x =
    case x of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


parseFormDate : String -> Maybe ( Int, Month, Int )
parseFormDate dateString =
    case dateString |> String.split "-" |> List.map String.toInt of
        (Just year) :: (Just monthInt) :: (Just day) :: [] ->
            case monthInt |> monthFromInt of
                Just month ->
                    Just ( year, month, day )

                Nothing ->
                    Nothing

        _ ->
            Nothing


parseFormDateString : Common -> String -> Maybe Posix
parseFormDateString common dateString =
    dateString
        |> parseFormDate
        |> Maybe.map
            (\( year, month, day ) ->
                partsToPosix common.timeZone
                    { year = year
                    , month = month
                    , day = day
                    , hour = 0
                    , minute = 0
                    , second = 0
                    , millisecond = 0
                    }
            )


parseFormDateAndTimeString : Common -> String -> String -> Maybe Posix
parseFormDateAndTimeString common dateString timeString =
    case
        ( dateString |> parseFormDate
        , timeString |> String.split ":" |> List.map String.toInt
        )
    of
        ( Just ( year, month, day ), (Just hour) :: (Just minute) :: [] ) ->
            Just <|
                partsToPosix common.timeZone
                    { year = year
                    , month = month
                    , day = day
                    , hour = hour
                    , minute = minute
                    , second = 0
                    , millisecond = 0
                    }

        _ ->
            Nothing
