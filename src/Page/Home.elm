module Page.Home exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.EventTimeline exposing (timeline)
import Datetime exposing (dateFormatter, fullDateTimeFormatter)
import Error exposing (GreaseResult)
import Graph exposing (HoveredEvent, graphGrades)
import Html exposing (Html, a, br, div, em, i, p, section, span, strong, text)
import Html.Attributes exposing (class, href, style)
import Json.Decode as Decode
import List.Extra exposing (last)
import Maybe.Extra exposing (filter, isJust, isNothing)
import Models.Event exposing (Event, eventDecoder)
import Models.Info exposing (Enrollment)
import Task
import Time exposing (Zone)
import Utils exposing (Common, RemoteData(..), eventIsOver, getRequest, resultToRemote, romanNumeral, roundToTwoDigits)



---- MODEL ----


type alias Model =
    { common : Common
    , events : RemoteData (List Event)
    , hoveredEvent : Maybe HoveredEvent
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common, events = Loading, hoveredEvent = Nothing }, loadEvents common )



---- UPDATE ----


type Msg
    = OnLoadEvents (GreaseResult (List Event))
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
    getRequest common "/events?full=true" (Decode.list eventDecoder)
        |> Task.attempt OnLoadEvents


attendanceMessage : Maybe Enrollment -> Float -> String
attendanceMessage enrollment finalGrade =
    if isNothing enrollment then
        "Do you even go here?"

    else if finalGrade >= 90.0 then
        "Ayy lamo nice."

    else if finalGrade >= 80.0 then
        "Ok not bad, I guess."

    else if finalGrade >= 70.0 then
        "Pls"

    else
        "BRUH get it together."



---- VIEW ----


view : Model -> Html Msg
view model =
    model.events
        |> Basics.remoteContent (homePage model)


homePage : Model -> List Event -> Html Msg
homePage model events =
    let
        ( pastEvents, futureEvents ) =
            events
                |> List.partition (eventIsOver model.common)
                |> Tuple.mapFirst (List.filter (\event -> isJust event.gradeChange))

        gigRequirement =
            model.common.currentSemester.gigRequirement
    in
    div []
        [ gradesBlock model.common pastEvents
        , model.hoveredEvent
            |> Maybe.map (eventHoverBox model.common)
            |> Maybe.withDefault (text "")
        , Basics.section
            [ Basics.container
                [ Basics.columns
                    [ upcomingEvents model.common futureEvents
                    , volunteerism model.common.timeZone pastEvents gigRequirement
                    ]
                ]
            ]
        ]


gradesBlock : Common -> List Event -> Html Msg
gradesBlock common pastEvents =
    let
        finalGrade =
            pastEvents
                |> List.Extra.last
                |> Maybe.andThen .gradeChange
                |> Maybe.map .partialScore
                |> Maybe.withDefault 100.0

        attendanceIssueEmail =
            "mailto:gleeclub_officers@lists.gatech.edu?subject=Attendance%20Issue"

        enrollment =
            common.user
                |> Maybe.andThen .enrollment

        scoreText =
            p []
                [ text "Right now you have a "
                , strong [] [ text <| String.fromFloat finalGrade ]
                , text "."
                , br [] []
                , span [ class "has-text-grey-light is-italic" ]
                    [ text <| attendanceMessage enrollment finalGrade ]
                ]

        graph =
            if List.length pastEvents == 0 then
                [ p [] [ text "New semester, new you! Make it count." ]
                , br [] []
                , br [] []
                ]

            else
                [ graphGrades common.currentSemester pastEvents HoverOverEvent
                , p []
                    [ br [] []
                    , text "Do you have an issue? Do you need a daddy tissue? "
                    , a [ href attendanceIssueEmail ]
                        [ text "Email the officers" ]
                    , text " to cry about it."
                    ]
                ]
    in
    Basics.section
        [ Basics.container
            (Basics.title "Score" :: scoreText :: graph)
        ]


eventHoverBox : Common -> HoveredEvent -> Html Msg
eventHoverBox common event =
    let
        gradeChange =
            event.event.gradeChange
                |> Maybe.map .change
                |> Maybe.withDefault 0.0
                |> roundToTwoDigits
                |> String.fromFloat

        partialScore =
            event.event.gradeChange
                |> Maybe.map .partialScore
                |> Maybe.withDefault 0.0
                |> roundToTwoDigits
                |> String.fromFloat

        changeReason =
            event.event.gradeChange
                |> Maybe.map .reason
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


upcomingEvents : Common -> List Event -> Html Msg
upcomingEvents common futureEvents =
    Basics.column
        [ Basics.title "This Week"
        , Basics.box
            [ timeline common futureEvents ]
        ]


volunteerism : Zone -> List Event -> Int -> Html Msg
volunteerism timeZone pastEvents gigRequirement =
    let
        attendedVolunteerEvent : Event -> Bool
        attendedVolunteerEvent event =
            event.gigCount && (event.attendance |> Maybe.map .didAttend |> Maybe.withDefault False)

        volunteerGigsAttended =
            pastEvents |> List.filter attendedVolunteerEvent

        ( prelude, gigList ) =
            if List.length volunteerGigsAttended >= gigRequirement then
                ( p []
                    [ text "The dedication! The passion! The attendance! You've been to "
                    , text <| romanNumeral (List.length volunteerGigsAttended)
                    , text " volunteer gigs this semester. "
                    , text "Glub salutes you and your volunteerism."
                    ]
                , volunteerGigsAttended |> List.map Just
                )

            else
                ( p []
                    [ text "OK so you've only been to "
                    , text <| romanNumeral (List.length volunteerGigsAttended)
                    , text " volunteer gigs this semester and you need to go to "
                    , text <| romanNumeral gigRequirement
                    , text ". So. Uh, you know, do that."
                    ]
                , ((volunteerGigsAttended |> List.map Just) ++ List.repeat gigRequirement Nothing)
                    |> List.take gigRequirement
                )
    in
    Basics.column
        [ Basics.title "Volunteerism"
        , Basics.box
            [ prelude
            , p [ style "text-align" "center" ]
                (gigList
                    |> List.map (gigIcon timeZone)
                    |> List.intersperse (text " ")
                )
            ]
        ]


gigIcon : Zone -> Maybe Event -> Html Msg
gigIcon timeZone maybeGig =
    case maybeGig of
        Just gig ->
            span
                (class "icon is-large has-text-primary"
                    :: (Basics.multilineTooltip <|
                            gig.name
                                ++ " on "
                                ++ dateFormatter timeZone gig.callTime
                       )
                )
                [ i [ class "far fa-2x fa-check-circle" ] [] ]

        Nothing ->
            span
                (class "icon is-large"
                    :: style "color" "gray"
                    :: Basics.tooltip "Hopefully something soon..."
                )
                [ i [ class "far fa-2x fa-frown" ] [] ]
