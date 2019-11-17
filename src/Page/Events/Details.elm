module Page.Events.Details exposing (Model, Msg(..), attendance, init, loadEvent, onRsvp, rsvp, rsvpActions, uniform, update, view, viewEventDetails)

import Html exposing (Html, a, b, br, div, h1, i, img, p, section, span, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (attribute, class, href, id, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, maybe, nullable, string)
import Maybe.Extra
import Models.Event exposing (FullEvent, FullEventAttendance, fullEventDecoder)
import Models.Info exposing (Uniform)
import Route exposing (Route)
import Task
import Time exposing (Posix, posixToMillis)
import Utils exposing (Common, RemoteData(..), eventIsOver, fullDateTimeFormatter, getRequest, spinner, timeFormatter)



---- MODEL ----


type alias Model =
    { event : RemoteData FullEvent
    , confirmedAttendance : Bool
    , common : Common
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { event = Loading, confirmedAttendance = False, common = common }, loadEvent common eventId )



---- UPDATE ----


type Msg
    = OnLoadEvent (Result Http.Error FullEvent)
    | Rsvp Bool
    | OnRsvp (Result Http.Error Bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadEvent (Ok event) ->
            ( { model | event = Loaded event }, Cmd.none )

        OnLoadEvent (Err _) ->
            ( { model | event = Failure }, Cmd.none )

        Rsvp attending ->
            let
                cmd =
                    case model.event of
                        Loaded event ->
                            rsvp model.common event.id attending

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        OnRsvp requestResult ->
            ( model |> onRsvp requestResult, Cmd.none )



---- DATA ----


loadEvent : Common -> Int -> Cmd Msg
loadEvent common eventId =
    let
        url =
            "/events/" ++ String.fromInt eventId
    in
    getRequest common url (Http.expectJson OnLoadEvent fullEventDecoder)


rsvp : Common -> Int -> Bool -> Cmd Msg
rsvp common eventId attending =
    let
        url =
            "/events/"
                ++ String.fromInt eventId
                ++ "/rsvp/"
                ++ (if attending then
                        "true"

                    else
                        "false"
                   )
    in
    getRequest common url (Http.expectJson OnRsvp (Decode.succeed attending))


onRsvp : Result Http.Error Bool -> Model -> Model
onRsvp requestResult model =
    let
        ( updatedEvent, confirmedAttendance ) =
            case ( requestResult, model.event ) of
                ( Ok attending, Loaded event ) ->
                    case event.attendance of
                        Just eventAttendance ->
                            ( Loaded { event | attendance = Just { eventAttendance | confirmed = attending } }, True )

                        Nothing ->
                            ( Loaded event, True )

                ( Err _, event ) ->
                    ( event, False )

                ( _, event ) ->
                    ( event, True )
    in
    { model | event = updatedEvent, confirmedAttendance = confirmedAttendance }



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.event of
        NotAsked ->
            text ""

        Loading ->
            spinner

        Loaded event ->
            viewEventDetails model.common model.confirmedAttendance event

        Failure ->
            text "whoops"


viewEventDetails : Common -> Bool -> FullEvent -> Html Msg
viewEventDetails common confirmedAttendance event =
    let
        title =
            h1 [ class "title is-3" ] [ text event.name ]

        subtitle =
            p [ class "subtitle is-5" ] <|
                [ text
                    (event.callTime
                        |> fullDateTimeFormatter
                            common.timeZone
                    )
                , br [] []
                ]
                    ++ (event.location
                            |> Maybe.map (\l -> [ text l ])
                            |> Maybe.withDefault []
                       )

        maybeComments =
            event.comments
                |> Maybe.map
                    (\comments ->
                        [ p [] [ text comments, br [] [], br [] [] ] ]
                    )

        performAt =
            event.gig
                |> Maybe.map .performanceTime
                |> Maybe.map
                    (\performanceTime ->
                        [ p []
                            [ text "Perform at: "
                            , text <|
                                timeFormatter
                                    common.timeZone
                                    performanceTime
                            ]
                        ]
                    )

        points =
            p []
                [ text "This event is worth "
                , b [] [ text <| String.fromInt event.points, text " points" ]
                ]

        maybeSection =
            event.section
                |> Maybe.map
                    (\section ->
                        [ p [] [ text <| "This event is for the " ++ section ++ " section" ] ]
                    )

        rsvpIssue issue =
            [ p [ class "has-text-grey-light is-italic" ] [ text issue ] ]

        requestAbsence =
            -- TODO: why do we need rsvpIssue
            event.rsvpIssue
                |> Maybe.Extra.filter
                    (\_ ->
                        not <|
                            eventIsOver
                                common.now
                                event
                    )
                |> Maybe.map
                    (\_ ->
                        a
                            [ class "button is-primary is-outlined"
                            , Route.href <| Route.Events { id = Just event.id, tab = Just Route.EventRequestAbsence }
                            ]
                            [ text "Request Absence" ]
                    )

        attendanceBlock =
            let
                content =
                    case
                        ( event.attendance
                        , event.rsvpIssue
                        , eventIsOver
                            common.now
                            event
                        )
                    of
                        ( Just eventAttendance, _, True ) ->
                            [ span [] <| attendance eventAttendance ]

                        ( _, Just issue, False ) ->
                            [ span [] <| rsvpIssue issue ]

                        ( Just eventAttendance, Nothing, False ) ->
                            [ span [] <| rsvpActions eventAttendance ]

                        ( Nothing, _, _ ) ->
                            []
            in
            div [] content
    in
    div [] <|
        List.concat
            [ [ title, subtitle ]
            , Maybe.withDefault [] maybeComments
            , [ attendanceBlock ]
            , Maybe.withDefault [] performAt
            , [ points ]
            , maybeSection |> Maybe.withDefault []
            , event.gig |> Maybe.map .uniform |> Maybe.map uniform |> Maybe.withDefault []
            ]


rsvpActions : FullEventAttendance -> List (Html Msg)
rsvpActions eventAttendance =
    case ( eventAttendance.confirmed, eventAttendance.shouldAttend ) of
        ( True, True ) ->
            [ p []
                [ text "You're "
                , b [] [ text "confirmed" ]
                , text " to be "
                , b [] [ text "attending" ]
                ]
            , a [ class "button is-primary is-outlined", onClick <| Rsvp False ]
                [ text "oops jk, gotta dip" ]
            ]

        ( True, False ) ->
            [ p [] [ text "The officers know you won't be there" ]
            , a [ class "button is-primary" ] [ text "sike I can come. put me in coach!" ]
            ]

        ( False, True ) ->
            [ p [] [ text "You're coming, right?" ]
            , a [ class "button is-primary is-outlined", onClick <| Rsvp False ]
                [ text "sorry fam, not this time" ]
            , a [ class "button is-primary", onClick <| Rsvp True ]
                [ text "yep, I'll be there" ]
            ]

        ( False, False ) ->
            [ p [] [ text "You're not coming, right?" ]
            , a [ class "button is-primary is-outlined", onClick <| Rsvp False ]
                [ text "akshually I can come. you're welcome" ]
            , a [ class "button is-primary" ] [ text "nah, I'm gonna miss it" ]
            ]


attendance : FullEventAttendance -> List (Html Msg)
attendance eventAttendance =
    case ( eventAttendance.didAttend, eventAttendance.shouldAttend ) of
        ( True, True ) ->
            [ text "You were there! What a great time. Real #tbt material." ]

        ( True, False ) ->
            [ text "Wow, thanks for coming. What a guy!" ]

        ( False, True ) ->
            [ text "You "
            , b [] [ text "weren't there" ]
            , text ", and that's "
            , b [] [ text "not ok" ]
            , text ". You lost "
            , text <| String.fromFloat <| abs eventAttendance.gradeChange
            , text " points. "
            , a [ href "mailto:gleeclub_officers@lists.gatech.edu?subject=Attendance%20Issue" ]
                [ text "Email the officers" ]
            , text " if you think that's not right."
            ]

        ( False, False ) ->
            [ text "You "
            , b [] [ text "weren't there" ]
            , text ", but that's "
            , b [] [ text "ok" ]
            , text "."
            ]


uniform : Uniform -> List (Html Msg)
uniform eventUniform =
    [ p []
        [ span [] [ text eventUniform.name ]
        , span
            [ style "cursor" "pointer"
            , class "icon tooltip is-tooltip-multiline has-text-grey-light is-small"
            , attribute "data-tooltip" (eventUniform.description |> Maybe.withDefault "")
            ]
            [ i [ class "far fa-question-circle" ] [] ]
        , br [] []
        ]
    ]
