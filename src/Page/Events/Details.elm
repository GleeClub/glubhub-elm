module Page.Events.Details exposing (Model, Msg(..), init, update, view)

import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, div, h1, i, img, p, section, span, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (attribute, class, href, id, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, maybe, nullable, string)
import Json.Encode as Encode
import Maybe.Extra
import Models.Event exposing (FullEvent, FullEventAttendance, fullEventDecoder)
import Models.Info exposing (Uniform)
import Route exposing (Route)
import Task
import Time exposing (Posix)
import Utils exposing (Common, RemoteData(..), alert, eventIsOver, fullDateTimeFormatter, getRequest, postRequest, timeFormatter)



---- MODEL ----


type alias Model =
    { common : Common
    , event : FullEvent
    , confirmedAttendance : Bool
    }


init : Common -> FullEvent -> ( Model, Cmd Msg )
init common event =
    ( { common = common
      , event = event
      , confirmedAttendance = False
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
            ( model, rsvp model.common model.event.id attending )

        OnRsvp requestResult ->
            ( model |> onRsvp requestResult, Cmd.none )



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
    in
    getRequest common url (Decode.succeed attending) |> Task.attempt OnRsvp


onRsvp : GreaseResult Bool -> Model -> Model
onRsvp requestResult model =
    let
        event =
            model.event

        ( updatedEvent, confirmedAttendance ) =
            case requestResult of
                Ok attending ->
                    case model.event.attendance of
                        Just eventAttendance ->
                            ( { event | attendance = Just { eventAttendance | confirmed = attending } }, True )

                        Nothing ->
                            ( event, True )

                Err _ ->
                    ( event, False )
    in
    { model | event = updatedEvent, confirmedAttendance = confirmedAttendance }



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
            , [ attendanceBlock model.common model.event ]
            , model.event.gig
                |> Maybe.map .performanceTime
                |> maybeToList
                    (\performanceTime ->
                        p []
                            [ text "Perform at: "
                            , text <|
                                timeFormatter
                                    model.common.timeZone
                                    performanceTime
                            ]
                    )
            , [ p []
                    [ text "This event is worth "
                    , b [] [ text <| String.fromInt model.event.points, text " points" ]
                    ]
              ]
            , model.event.section
                |> maybeToList
                    (\section ->
                        p [] [ text <| "This event is for the " ++ section ++ " section" ]
                    )
            , model.event.gig |> Maybe.map .uniform |> maybeToList uniformSection
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


attendanceBlock : Common -> FullEvent -> Html Msg
attendanceBlock common event =
    case ( event.attendance, event.rsvpIssue, eventIsOver common.now event ) of
        ( Just eventAttendance, _, True ) ->
            span [] <| attendance eventAttendance

        ( _, Just issue, False ) ->
            span [] <| [ rsvpIssueSection issue ]

        ( Just eventAttendance, Nothing, False ) ->
            span [] <| rsvpActions eventAttendance

        ( Nothing, _, _ ) ->
            span [] []


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
            , a [ class "button is-primary", onClick <| Rsvp True ]
                [ text "sike I can come. put me in coach!" ]
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
