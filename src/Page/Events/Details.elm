module Page.Events.Details exposing (InternalMsg, Model, Msg, Translator, init, translator, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.DeleteModal exposing (deleteModal)
import Datetime exposing (fullDateTimeFormatter, timeFormatter)
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, i, p, span, text, u)
import Html.Attributes exposing (attribute, class, href, style, target)
import Json.Encode as Encode
import List.Extra exposing (find)
import Maybe.Extra as Maybe exposing (isNothing)
import Models.Admin exposing (AbsenceRequestState(..))
import Models.Event exposing (Event, Gig, SimpleAttendance)
import Models.Info exposing (Uniform)
import Permissions
import Request
import Route exposing (EventTab(..))
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), eventIsOver)



---- MODEL ----


type alias Model =
    { common : Common
    , event : Event
    , deleteState : Maybe SubmissionState
    , state : SubmissionState
    }


init : Common -> Event -> ( Model, Cmd Msg )
init common event =
    ( { common = common
      , event = event
      , deleteState = Nothing
      , state = NotSentYet
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ForSelf InternalMsg
    | ForParent OutMsg


type InternalMsg
    = Rsvp Bool
    | ConfirmAttending
    | OnRsvp (GreaseResult Bool)
    | TryToDeleteEvent
    | CancelDeleteEvent
    | SendDeleteEvent
    | OnDeleteEvent (GreaseResult ())


type OutMsg
    = DeleteEvent Int
    | EditEvent Event
    | SwitchTab EventTab


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onDeleteEvent : Int -> msg
    , onEditEvent : Event -> msg
    , onSwitchTab : EventTab -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator dictionary msg =
    case msg of
        ForSelf internal ->
            dictionary.onInternalMessage internal

        ForParent (DeleteEvent eventId) ->
            dictionary.onDeleteEvent eventId

        ForParent (EditEvent event) ->
            dictionary.onEditEvent event

        ForParent (SwitchTab tab) ->
            dictionary.onSwitchTab tab


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rsvp attending ->
            ( { model | state = Sending }, rsvp model.common model.event.id attending )

        ConfirmAttending ->
            ( { model | state = Sending }, confirm model.common model.event.id )

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
            ( { model | state = NotSentYet, event = { event | attendance = attendance } }
            , Task.perform (\_ -> ForParent <| EditEvent { event | attendance = attendance }) (Task.succeed ())
            )

        TryToDeleteEvent ->
            ( { model | deleteState = Just NotSentYet }, Cmd.none )

        CancelDeleteEvent ->
            ( { model | deleteState = Nothing }, Cmd.none )

        SendDeleteEvent ->
            ( { model | deleteState = Just Sending }, deleteEvent model.common model.event.id )

        OnDeleteEvent (Ok _) ->
            ( model, Task.perform (\_ -> ForParent <| DeleteEvent model.event.id) (Task.succeed ()) )

        OnDeleteEvent (Err error) ->
            ( { model | deleteState = Just <| ErrorSending error }, Cmd.none )



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
    Request.post common url emptyBody
        |> Task.attempt
            (\result ->
                ForSelf <|
                    OnRsvp
                        (result |> Result.map (\_ -> attending))
            )


confirm : Common -> Int -> Cmd Msg
confirm common eventId =
    let
        url =
            "/events/" ++ String.fromInt eventId ++ "/confirm"

        emptyBody =
            Encode.object []
    in
    Request.post common url emptyBody
        |> Task.attempt
            (\result ->
                ForSelf <|
                    OnRsvp
                        (result |> Result.map (\_ -> True))
            )


deleteEvent : Common -> Int -> Cmd Msg
deleteEvent common eventId =
    let
        url =
            "/events/" ++ String.fromInt eventId
    in
    Request.delete common url
        |> Task.attempt (ForSelf << OnDeleteEvent)



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
            , [ span [] <| attendanceBlock model ]
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
                |> Maybe.andThen (\id -> model.common.info.uniforms |> find (\uniform -> uniform.id == id))
                |> maybeToList uniformSection
            , [ absenceRequestButton model.common model.event ]
            , [ officerInfoSection model ]
            , [ case model.state of
                    ErrorSending error ->
                        Basics.errorBox error

                    _ ->
                        text ""
              ]
            ]


subtitleAndLocation : Common -> Event -> Html Msg
subtitleAndLocation common event =
    p [ class "subtitle is-5" ] <|
        [ text (event.callTime |> fullDateTimeFormatter common.timeZone)
        , br [] []
        , case event.location of
            Just location ->
                a
                    [ href <| "https://www.google.com/maps/search/" ++ location
                    , target "_blank"
                    ]
                    [ text location ]

            Nothing ->
                text ""
        ]


maybeToList : (a -> b) -> Maybe a -> List b
maybeToList mapper data =
    data |> Maybe.map (\d -> [ mapper d ]) |> Maybe.withDefault []


rsvpIssueSection : String -> Html Msg
rsvpIssueSection issue =
    p [ class "has-text-grey-light is-italic" ] [ text issue ]


attendanceBlock : Model -> List (Html Msg)
attendanceBlock model =
    case ( model.event.attendance, model.event.rsvpIssue, model.event |> eventIsOver model.common ) of
        ( Just attendance, _, True ) ->
            attendanceSummary model.event.points attendance

        ( attendance, Just issue, False ) ->
            if [ "Sectional", "Tutti Gig", "Rehearsal" ] |> List.member model.event.type_ then
                if attendance |> Maybe.map .confirmed |> Maybe.withDefault False then
                    [ text "We know you're coming." ]

                else
                    [ p [] [ text "You're coming, right?" ]
                    , Buttons.button
                        { content = "yep, I'll be there"
                        , onClick = Just <| ForSelf ConfirmAttending
                        , attrs =
                            [ Buttons.Color Buttons.IsPrimary
                            , Buttons.IsLoading (model.state == Sending)
                            ]
                        }
                    ]

            else
                [ rsvpIssueSection issue ]

        ( Just attendance, Nothing, False ) ->
            attendance |> rsvpActions (model.state == Sending)

        ( Nothing, _, _ ) ->
            []


rsvpActions : Bool -> SimpleAttendance -> List (Html Msg)
rsvpActions isSending attendance =
    let
        rsvpButton attending content =
            Buttons.button
                { content = content
                , onClick = Just <| ForSelf <| Rsvp attending
                , attrs =
                    [ Just <| Buttons.Color Buttons.IsPrimary
                    , Just <| Buttons.IsLoading isSending
                    , Just Buttons.IsOutlined
                        |> Maybe.filter (\_ -> not attending)
                    ]
                        |> List.filterMap identity
                }
    in
    case ( attendance.confirmed, attendance.shouldAttend ) of
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
            ]


attendanceSummary : Int -> SimpleAttendance -> List (Html Msg)
attendanceSummary eventPoints attendance =
    case ( attendance.didAttend, attendance.shouldAttend ) of
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
            , text <| String.fromInt eventPoints
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
        , text " "
        , span
            [ style "cursor" "pointer"
            , class "icon tooltip has-tooltip-bottom is-tooltip-multiline has-text-grey-light is-small"
            , attribute "data-tooltip" (uniform.description |> Maybe.withDefault "")
            ]
            [ i [ class "far fa-question-circle" ] [] ]
        , br [] []
        ]


absenceRequestButton : Common -> Event -> Html Msg
absenceRequestButton common event =
    if (event |> eventIsOver common) && isNothing event.rsvpIssue then
        text ""

    else
        let
            ( content, action ) =
                case event.absenceRequest |> Maybe.map .state of
                    Nothing ->
                        ( "Request Absence", Just <| ForParent <| SwitchTab EventRequestAbsence )

                    Just AbsenceRequestPending ->
                        ( "Request Submitted", Nothing )

                    Just AbsenceRequestApproved ->
                        ( "Request Approved", Nothing )

                    Just AbsenceRequestDenied ->
                        ( "Request Denied", Nothing )
        in
        Buttons.button
            { content = content
            , onClick = action
            , attrs = [ Buttons.Color Buttons.IsPrimary, Buttons.IsOutlined ]
            }


officerInfoSection : Model -> Html Msg
officerInfoSection model =
    Basics.renderIfHasPermission model.common Permissions.viewEventPrivateDetails <|
        div []
            [ Basics.divider ""
            , model.event.gig
                |> Maybe.map contactInfo
                |> Maybe.withDefault (text "")
            , model.event.gig
                |> Maybe.map priceInfo
                |> Maybe.withDefault (text "")
            , br [] []
            , Buttons.button
                { content = "Edit this bitch"
                , onClick = Just <| ForParent <| SwitchTab EventEdit
                , attrs = [ Buttons.CustomAttrs [ style "margin-bottom" "5px" ] ]
                }
            , br [] []
            , Buttons.button
                { content = "Yeet this bitch into the void"
                , onClick = Just <| ForSelf TryToDeleteEvent
                , attrs = [ Buttons.Color Buttons.IsDanger, Buttons.IsOutlined ]
                }
            , model.deleteState
                |> Maybe.map (deleteEventModal model.event)
                |> Maybe.withDefault (text "")
            ]


deleteEventModal : Event -> SubmissionState -> Html Msg
deleteEventModal event state =
    deleteModal
        { title = "Delete " ++ event.name ++ "?"
        , cancel = ForSelf CancelDeleteEvent
        , confirm = ForSelf SendDeleteEvent
        , state = state
        , content =
            p []
                [ text "Are you sure you want to delete this event? "
                , text "Once you delete it, it's gone like Donkey Kong."
                ]
        }


contactInfo : Gig -> Html Msg
contactInfo gig =
    if
        isNothing gig.contactName
            && isNothing gig.contactEmail
            && isNothing gig.contactPhone
    then
        p [] [ i [] [ text "No contact info" ] ]

    else
        p []
            [ u [] [ text "Contact" ]
            , br [] []
            , gig.contactName
                |> Maybe.map text
                |> Maybe.withDefault (i [] [ text "idk who" ])
            , br [] []
            , gig.contactPhone
                |> Maybe.map Basics.phoneLink
                |> Maybe.withDefault (i [] [ text "no number, bro" ])
            , br [] []
            , gig.contactEmail
                |> Maybe.map Basics.emailLink
                |> Maybe.withDefault (i [] [ text "no email, dude" ])
            ]


priceInfo : Gig -> Html Msg
priceInfo gig =
    case gig.price of
        Just price ->
            p [] [ text <| "$" ++ String.fromInt price ]

        Nothing ->
            text ""
