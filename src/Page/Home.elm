module Page.Home exposing (Model, Msg(..), attendanceMessage, gradesBlock, init, loadEvents, pastAndFutureEvents, upcomingEvents, update, view, volunteerism)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Graph exposing (EventHovered, graphGrades)
import Html exposing (Html, a, br, button, div, form, h1, i, img, input, label, p, section, span, strong, text)
import Html.Attributes exposing (attribute, class, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (find, last)
import Maybe.Extra exposing (filter, isJust)
import Models.Event exposing (FullEvent, Grades, fullEventDecoder)
import Models.Info exposing (Semester)
import Route exposing (Route)
import Time exposing (Zone, posixToMillis)
import Utils exposing (Common, RemoteData(..), dateFormatter, getRequest, notFoundView, permittedTo, romanNumeral, setToken, spinner, timeFromNow)



---- MODEL ----


type alias Model =
    { common : Common
    , events : RemoteData (List FullEvent)
    , hoveredEvent : Maybe EventHovered
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common, events = Loading, hoveredEvent = Nothing }, loadEvents common )



---- UPDATE ----


type Msg
    = OnLoadEvents (Result Http.Error (List FullEvent))
    | HoverOverEvent (Maybe EventHovered)
    | ClearHover


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadEvents (Ok events) ->
            ( { model | events = Loaded events }, Cmd.none )

        OnLoadEvents (Err _) ->
            ( { model | events = Failure }, Cmd.none )

        HoverOverEvent maybeEvent ->
            ( { model | hoveredEvent = maybeEvent }, Cmd.none )

        ClearHover ->
            ( { model | hoveredEvent = Nothing }, Cmd.none )



---- DATA ----


loadEvents : Common -> Cmd Msg
loadEvents common =
    getRequest common "/events?full=true" (Http.expectJson OnLoadEvents (Decode.list <| fullEventDecoder))


pastAndFutureEvents : Common -> List FullEvent -> ( List FullEvent, List FullEvent )
pastAndFutureEvents common allEvents =
    let
        allGrades =
            common.user
                |> Maybe.andThen .grades
                |> Maybe.map .changes
                |> Maybe.withDefault []

        mostRecentPastEvent =
            allGrades
                |> List.Extra.last
                |> Maybe.map .event

        mostRecentCallTime =
            mostRecentPastEvent |> Maybe.map .callTime
    in
    case mostRecentCallTime of
        Just callTime ->
            allEvents |> List.partition (\e -> posixToMillis e.callTime <= posixToMillis callTime)

        _ ->
            ( allEvents, [] )


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



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.events of
        NotAsked ->
            text ""

        Loading ->
            spinner

        Failure ->
            text "Shucks!"

        Loaded events ->
            let
                ( pastEvents, futureEvents ) =
                    pastAndFutureEvents model.common events

                gigRequirement =
                    model.common.user
                        |> Maybe.andThen .grades
                        |> Maybe.map .gigRequirement
                        |> Maybe.withDefault 5

                maybeGrades =
                    model.common.user |> Maybe.andThen .grades
            in
            div [ id "home" ]
                [ gradesBlock model.common.currentSemester maybeGrades pastEvents
                , section [ class "section" ]
                    [ div [ class "container" ]
                        [ div [ class "columns" ]
                            [ upcomingEvents model.common futureEvents
                            , volunteerism model.common.timeZone pastEvents gigRequirement
                            ]
                        ]
                    ]
                ]


gradesBlock : Semester -> Maybe Grades -> List FullEvent -> Html Msg
gradesBlock semester grades pastEvents =
    let
        finalGrade =
            pastEvents
                |> List.Extra.last
                |> Maybe.andThen .attendance
                |> Maybe.map .partialScore
                |> Maybe.withDefault 100.0

        roundedFinalGrade =
            toFloat (round (finalGrade * 100.0)) / 100.0

        attendanceIssueEmail =
            "mailto:gleeclub_officers@lists.gatech.edu?subject=Attendance%20Issue"

        scoreText =
            p []
                [ text "Right now you have a"
                , strong [] [ text <| String.fromFloat finalGrade ]
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
            ([ Basics.title "Score"
             , p []
                [ text "Right now you have a "
                , strong [] [ text <| String.fromFloat roundedFinalGrade ]
                , text "."
                , br [] []
                , span [ class "has-text-grey-light is-italic" ]
                    [ text <| attendanceMessage grades ]
                ]
             ]
                ++ graph
            )
        ]


upcomingEvents : Common -> List FullEvent -> Html Msg
upcomingEvents common futureEvents =
    let
        singleEvent index event =
            p []
                [ span [ class "tag is-primary is-rounded" ]
                    [ text <| String.fromInt (index + 1) ]
                , a [ Route.href <| Route.Events { id = Just event.id, tab = Nothing } ]
                    [ text event.name
                    , text "–"
                    , text <| timeFromNow common event.callTime
                    ]
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
                                [ class "icon is-large tooltip is-tooltip has-text-primary"
                                , Basics.tooltip <| tooltipText gig
                                ]
                                [ gigIcon gig ]
                        )
                )
            ]
        ]
