module Components.EventTimeline exposing (timeline)

import Components.Basics exposing (tooltip)
import Datetime exposing (dateFormatter, fullDateTimeFormatter)
import Html exposing (Html, a, div, header, i, p, span, text)
import Html.Attributes exposing (class, href)
import List.Extra as List
import Models.Event exposing (Event)
import Route
import Time exposing (Weekday(..), posixToMillis)
import Time.Extra as Time
import Utils exposing (Common)


timeline : Common -> List Event -> Html msg
timeline common futureEvents =
    let
        weekOfEvents =
            groupIntoWeek common futureEvents

        firstWeekday =
            common.now
                |> Time.toWeekday common.timeZone

        nextWeekStart =
            "Next " ++ weekdayToString firstWeekday
    in
    div [ class "timeline is-centered" ]
        (((weekOfEvents
            |> List.indexedMap
                (\index ( day, events ) ->
                    let
                        dayName =
                            if index == 0 then
                                weekdayToString day ++ " (Today)"

                            else
                                weekdayToString day

                        dayHeader =
                            timelineHeader dayName
                    in
                    if List.isEmpty events then
                        [ dayHeader, nothingOnDay day ]

                    else
                        dayHeader :: (events |> List.map (timelineEvent common))
                )
          )
            |> List.concat
         )
            ++ [ timelineHeader nextWeekStart ]
        )


groupIntoWeek : Common -> List Event -> List ( Weekday, List Event )
groupIntoWeek common events =
    let
        beginningOfWeek =
            common.now
                |> Time.floor Time.Day common.timeZone
                |> posixToMillis

        eventsDuringDay dayMillis =
            let
                nextDayMillis =
                    dayMillis + (1000 * 60 * 60 * 24)
            in
            events
                |> List.filter
                    (\e ->
                        let
                            callTime =
                                posixToMillis e.callTime
                        in
                        callTime >= dayMillis && callTime < nextDayMillis
                    )

        allDays =
            (1000 * 60 * 60 * 24)
                |> List.repeat 6
                |> List.scanl (+) beginningOfWeek
    in
    allDays
        |> List.map
            (\day ->
                ( Time.millisToPosix day |> Time.toWeekday common.timeZone
                , eventsDuringDay day
                )
            )


weekdayToString : Weekday -> String
weekdayToString weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


timelineHeader : String -> Html msg
timelineHeader content =
    header [ class "timeline-header" ]
        [ span [ class "tag is-primary" ]
            [ text content ]
        ]


timelineEvent : Common -> Event -> Html msg
timelineEvent common event =
    div [ class "timeline-item" ]
        [ div
            (class "timeline-marker is-primary"
                :: tooltip
                    (event.callTime
                        |> fullDateTimeFormatter common.timeZone
                    )
            )
            []
        , div [ class "timeline-content" ]
            [ p [ class "heading" ]
                [ text (event.callTime |> dateFormatter common.timeZone) ]
            , p []
                [ a [ Route.href <| Route.Events { id = Just event.id, tab = Nothing } ]
                    [ text event.name ]
                ]
            ]
        ]


nothingOnDay : Weekday -> Html msg
nothingOnDay weekday =
    div [ class "timeline-item" ]
        [ div [ class "timeline-marker" ] []
        , div [ class "timeline-content" ]
            [ p []
                [ i [] [ text <| "Nothing on " ++ weekdayToString weekday ]
                ]
            ]
        ]
