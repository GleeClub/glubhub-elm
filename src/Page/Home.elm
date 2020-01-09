module Page.Home exposing (Model, Msg(..), attendanceMessage, gradesBlock, init, loadEvents, pastAndFutureEvents, upcomingEvents, update, view, volunteerism)

import Components.Basics as Basics
import Datetime exposing (dateFormatter, fullDateTimeFormatter, timeFromNow)
import Error exposing (GreaseResult)
import Graph exposing (HoveredEvent, graphGrades)
import Html exposing (Html, a, br, div, em, i, p, section, span, strong, text)
import Html.Attributes exposing (class, href, id, style)
import Json.Decode as Decode
import List.Extra exposing (last)
import Maybe.Extra exposing (filter, isJust)
import Models.Event exposing (FullEvent, Grades, fullEventDecoder)
import Models.Info exposing (Semester)
import Route
import Task
import Time exposing (Zone, posixToMillis)
import Utils exposing (Common, RemoteData(..), getRequest, resultToRemote, romanNumeral)



---- MODEL ----


type alias Model =
    { common : Common
    , events : RemoteData (List FullEvent)
    , hoveredEvent : Maybe HoveredEvent
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common, events = Loading, hoveredEvent = Nothing }, loadEvents common )



---- UPDATE ----


type Msg
    = OnLoadEvents (GreaseResult (List FullEvent))
    | HoverOverEvent (Maybe HoveredEvent)
    | ClearHover


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadEvents eventsResult ->
            ( { model | events = resultToRemote eventsResult }, Cmd.none )

        HoverOverEvent maybeEvent ->
            let
                hoveredEvent =
                    case ( maybeEvent, model.hoveredEvent ) of
                        ( event, Nothing ) ->
                            event

                        ( Nothing, _ ) ->
                            Nothing

                        ( Just newEvent, Just currentEvent ) ->
                            Just newEvent |> Maybe.Extra.filter (\new -> new.event.id /= currentEvent.event.id)
            in
            ( { model | hoveredEvent = hoveredEvent }, Cmd.none )

        ClearHover ->
            ( { model | hoveredEvent = Nothing }, Cmd.none )



---- DATA ----


loadEvents : Common -> Cmd Msg
loadEvents common =
    getRequest common "/events?full=true" (Decode.list fullEventDecoder)
        |> Task.attempt OnLoadEvents


pastAndFutureEvents : Common -> List FullEvent -> ( List FullEvent, List FullEvent )
pastAndFutureEvents common allEvents =
    allEvents |> List.partition (\e -> posixToMillis e.callTime <= posixToMillis common.now)


attendanceMessage : Maybe Grades -> String
attendanceMessage maybeGrades =
    case maybeGrades of
        Nothing ->
            "Do you even go here?"

        Just grades ->
            if grades.finalGrade >= 90.0 then
                "Ayy lamo nice."

            else if grades.finalGrade >= 80.0 then
                "Ok not bad, I guess."

            else if grades.finalGrade >= 70.0 then
                "Pls"

            else
                "BRUH get it together."


roundToTwoDigits : Float -> Float
roundToTwoDigits x =
    toFloat (round (x * 100.0)) / 100.0



---- VIEW ----


view : Model -> Html Msg
view model =
    model.events
        |> (Basics.remoteContent <|
                \events ->
                    let
                        ( pastEvents, futureEvents ) =
                            pastAndFutureEvents model.common events

                        maybeGrades =
                            model.common.user |> Maybe.andThen .grades

                        gigRequirement =
                            maybeGrades
                                |> Maybe.map .gigRequirement
                                |> Maybe.withDefault 5
                    in
                    div []
                        [ gradesBlock model.common.currentSemester maybeGrades pastEvents
                        , eventHoverBox model.common model.hoveredEvent
                        , section [ class "section" ]
                            [ div [ class "container" ]
                                [ div [ class "columns" ]
                                    [ upcomingEvents model.common futureEvents
                                    , volunteerism model.common.timeZone pastEvents gigRequirement
                                    ]
                                ]
                            ]
                        ]
           )


gradesBlock : Semester -> Maybe Grades -> List FullEvent -> Html Msg
gradesBlock semester grades pastEvents =
    let
        finalGrade =
            pastEvents
                |> List.Extra.last
                |> Maybe.andThen .attendance
                |> Maybe.andThen .partialScore
                |> Maybe.withDefault 100.0

        attendanceIssueEmail =
            "mailto:gleeclub_officers@lists.gatech.edu?subject=Attendance%20Issue"

        scoreText =
            p []
                [ text "Right now you have a "
                , strong [] [ text <| String.fromFloat (roundToTwoDigits finalGrade) ]
                , text "."
                , br [] []
                , span [ class "has-text-grey-light is-italic" ]
                    [ text <| attendanceMessage grades ]
                ]

        graph =
            if List.length pastEvents == 0 then
                [ p [] [ text "New semester, new you! Make it count." ]
                , br [] []
                , br [] []
                ]

            else
                [ graphGrades semester pastEvents HoverOverEvent
                , p [ id "attendance-issue" ]
                    [ br [] []
                    , text "Do you have an issue? Do you need a daddy tissue? "
                    , a [ href attendanceIssueEmail ]
                        [ text "Email the officers" ]
                    , text " to cry about it."
                    ]
                ]
    in
    section [ class "section" ]
        [ div [ class "container", id "grades-container" ]
            ([ Basics.title "Score", scoreText ] ++ graph)
        ]


eventHoverBox : Common -> Maybe HoveredEvent -> Html Msg
eventHoverBox common hoveredEvent =
    case hoveredEvent of
        Nothing ->
            div [] []

        Just event ->
            let
                gradeChange =
                    event.event.attendance
                        |> Maybe.andThen .gradeChange
                        |> Maybe.withDefault 0.0
                        |> roundToTwoDigits
                        |> String.fromFloat

                partialScore =
                    event.event.attendance
                        |> Maybe.andThen .partialScore
                        |> Maybe.withDefault 0.0
                        |> roundToTwoDigits
                        |> String.fromFloat

                changeReason =
                    event.event.attendance
                        |> Maybe.andThen .gradeChangeReason
                        |> Maybe.withDefault ""
            in
            div
                [ class "box shown"
                , style "position" "absolute"
                , style "top" (String.fromInt (event.y + 10) ++ "px")
                , style "left" (String.fromInt event.x ++ "px")
                , style "transform" "translateX(-50%)"
                ]
                [ p [] [ strong [] [ text event.event.name ] ]
                , p [] [ text (event.event.callTime |> fullDateTimeFormatter common.timeZone) ]
                , p []
                    [ text <| gradeChange ++ " points "
                    , span [ class "icon is-primary has-text-primary" ]
                        [ i [ class "fas fa-arrow-right" ] [] ]
                    , text <| " " ++ partialScore ++ "%"
                    ]
                , p [] [ em [] [ text changeReason ] ]
                ]


upcomingEvents : Common -> List FullEvent -> Html Msg
upcomingEvents common futureEvents =
    let
        singleEvent index event =
            p []
                [ span [ class "tag is-primary is-rounded" ]
                    [ text <| String.fromInt (index + 1) ]
                , text " "
                , a [ Route.href <| Route.Events { id = Just event.id, tab = Nothing } ]
                    [ text event.name ]
                , text " – "
                , text <| timeFromNow common event.callTime
                ]

        eventList =
            if List.length futureEvents == 0 then
                [ p [] [ text "No more events this semester (:(" ] ]

            else
                futureEvents
                    |> List.take 5
                    |> List.indexedMap singleEvent
    in
    Basics.column
        [ Basics.title "Next Up"
        , Basics.box eventList
        ]


volunteerism : Zone -> List FullEvent -> Int -> Html Msg
volunteerism timeZone pastEvents gigRequirement =
    let
        attendedVolunteerEvent : FullEvent -> Bool
        attendedVolunteerEvent event =
            event.gigCount && (event.attendance |> Maybe.map .didAttend |> Maybe.withDefault False)

        volunteerGigsAttended =
            pastEvents |> List.filter attendedVolunteerEvent

        partialGigList =
            ((volunteerGigsAttended |> List.map Just) ++ List.repeat gigRequirement Nothing)
                |> List.take gigRequirement

        tooltipText maybeGig =
            maybeGig
                |> Maybe.map (\gig -> gig.name ++ " on " ++ dateFormatter timeZone gig.callTime)
                |> Maybe.withDefault "Hopefully something soon..."

        gigIcon maybeGig =
            i
                [ class <|
                    "fas fa-2x "
                        ++ (if isJust maybeGig then
                                "fa-check-circle"

                            else
                                "fa-frown"
                           )
                ]
                []
    in
    Basics.column
        [ Basics.title "Volunteerism"
        , Basics.box
            [ p []
                [ text "OK so you've only been to "
                , text <| romanNumeral (List.length volunteerGigsAttended)
                , text " volunteer gigs this semester and you need to go to "
                , text <| romanNumeral gigRequirement
                , text ". So. Uh, you know, do that."
                ]
            , p [ style "text-align" "center" ]
                (partialGigList
                    |> List.map
                        (\gig ->
                            span
                                (class "icon is-large has-text-primary"
                                    :: (Basics.tooltip <| tooltipText gig)
                                )
                                [ gigIcon gig ]
                        )
                )
            ]
        ]
