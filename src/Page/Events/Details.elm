module Page.Events.Details exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Datetime exposing (fullDateTimeFormatter, timeFormatter)
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, i, p, span, text)
import Html.Attributes exposing (attribute, class, href, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Event exposing (FullEvent, FullEventAttendance)
import Models.Info exposing (Uniform)
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), eventIsOver, postRequestFull)



---- MODEL ----


type alias Model =
    { common : Common
    , event : FullEvent
    , state : SubmissionState
    }


init : Common -> FullEvent -> ( Model, Cmd Msg )
init common event =
    ( { common = common
      , event = event
      , state = NotSentYet
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Rsvp Bool
    | OnRsvp (GreaseResult Bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rsvp attending ->
            ( { model | state = Sending }, rsvp model.common model.event.id attending )

        OnRsvp (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )

        OnRsvp (Ok attending) ->
            let
                event =
                    model.event

                attendance =
                    model.event.attendance
                        |> Maybe.map (\a -> { a | confirmed = True, shouldAttend = attending })
            in
            ( { model | state = NotSentYet, event = { event | attendance = attendance } }, Cmd.none )



---- DATA ----


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

        emptyBody =
            Encode.object []
    in
    Utils.postRequestFull common url emptyBody (Decode.succeed attending)
        |> Task.attempt OnRsvp



---- VIEW ----


view : Model -> Html Msg
view model =
    div [] <|
        List.concat
            [ [ subtitleAndLocation model.common model.event ]
            , model.event.comments
                |> maybeToList
                    (\comments ->
                        p [] [ text comments, br [] [], br [] [] ]
                    )
            , [ attendanceBlock model ]
            , model.event.gig
                |> Maybe.map .performanceTime
                |> maybeToList
                    (\performanceTime ->
                        p []
                            [ text "Perform at: "
                            , text
                                (performanceTime
                                    |> timeFormatter model.common.timeZone
                                )
                            ]
                    )
            , [ p []
                    [ text "This event is worth "
                    , b []
                        [ text <| String.fromInt model.event.points
                        , text " points"
                        ]
                    ]
              ]
            , model.event.section
                |> maybeToList
                    (\section ->
                        p [] [ text <| "This event is for the " ++ section ++ " section" ]
                    )
            , model.event.gig
                |> Maybe.map .uniform
                |> maybeToList uniformSection
            , [ case model.state of
                    ErrorSending error ->
                        Basics.errorBox error

                    _ ->
                        text ""
              ]
            ]


subtitleAndLocation : Common -> FullEvent -> Html Msg
subtitleAndLocation common event =
    p [ class "subtitle is-5" ] <|
        [ text (event.callTime |> fullDateTimeFormatter common.timeZone)
        , br [] []
        ]
            ++ (event.location |> maybeToList text)


maybeToList : (a -> b) -> Maybe a -> List b
maybeToList mapper data =
    data |> Maybe.map (\d -> [ mapper d ]) |> Maybe.withDefault []


rsvpIssueSection : String -> Html Msg
rsvpIssueSection issue =
    p [ class "has-text-grey-light is-italic" ] [ text issue ]


attendanceBlock : Model -> Html Msg
attendanceBlock model =
    case ( model.event.attendance, model.event.rsvpIssue, eventIsOver model.common.now model.event ) of
        ( Just eventAttendance, _, True ) ->
            span [] <| attendanceSummary model.event.points eventAttendance

        ( _, Just issue, False ) ->
            span [] <| [ rsvpIssueSection issue ]

        ( Just eventAttendance, Nothing, False ) ->
            span [] <| (eventAttendance |> rsvpActions (model.state == Sending))

        ( Nothing, _, _ ) ->
            span [] []


rsvpActions : Bool -> FullEventAttendance -> List (Html Msg)
rsvpActions isSending eventAttendance =
    let
        rsvpButton attending content =
            button
                [ class <|
                    "button is-primary"
                        ++ (if not attending then
                                " is-outlined"

                            else
                                ""
                           )
                        ++ Utils.isLoadingClass isSending
                , onClick <| Rsvp attending
                ]
                [ text content ]
    in
    case ( eventAttendance.confirmed, eventAttendance.shouldAttend ) of
        ( True, True ) ->
            [ p []
                [ text "You're "
                , b [] [ text "confirmed" ]
                , text " to be "
                , b [] [ text "attending" ]
                ]
            , rsvpButton False "oops jk, gotta dip"
            ]

        ( True, False ) ->
            [ p [] [ text "The officers know you won't be there" ]
            , rsvpButton True "sike I can come. put me in coach!"
            ]

        ( False, True ) ->
            [ p [] [ text "You're coming, right?" ]
            , rsvpButton False "sorry fam, not this time"
            , text " "
            , rsvpButton True "yep, I'll be there"
            ]

        ( False, False ) ->
            [ p [] [ text "You're not coming, right?" ]
            , rsvpButton True "akshually I can come. you're welcome"

            -- , a [ class "button is-primary" ] [ text "nah, I'm gonna miss it" ]
            ]


attendanceSummary : Int -> FullEventAttendance -> List (Html Msg)
attendanceSummary eventPoints eventAttendance =
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
            , text <| String.fromFloat <| abs (eventAttendance.gradeChange |> Maybe.withDefault (eventPoints |> toFloat))
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


uniformSection : Uniform -> Html Msg
uniformSection uniform =
    p []
        [ span [] [ text uniform.name ]
        , span
            [ style "cursor" "pointer"
            , class "icon tooltip has-tooltip-bottom is-tooltip-multiline has-text-grey-light is-small"
            , attribute "data-tooltip" (uniform.description |> Maybe.withDefault "")
            ]
            [ i [ class "far fa-question-circle" ] [] ]
        , br [] []
        ]
