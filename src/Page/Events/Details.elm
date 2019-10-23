module Page.Events.Details exposing (attendance, rsvpActions, uniform, viewEventDetails)

import Html exposing (Html, a, div, h1, img, section, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, href, id, src)
import Maybe.Extra
import Models.Event exposing (FullEvent)
import Page.Events exposing (Model, Msg)
import Time exposing (Posix)


viewEventDetails : Model -> FullEvent -> Html Msg
viewEventDetails model event =
    let
        title =
            h1 [ class "title is-3" ] [ text event.name ]

        subtitle =
            p [ class "subtitle is-5" ]
                [ text <| formatCallTime event.callTime, br, text event.location ]

        comments =
            event.comments
                |> Maybe.map
                    (\comments ->
                        [ p [] [ text comments, br, br ] ]
                    )

        performAt =
            event.performanceTime
                |> Maybe.map
                    (\performanceTime ->
                        [ p []
                            [ text "Perform at: "
                            , text <| formatPerformanceTime performanceTime
                            ]
                        ]
                    )

        points =
            p []
                [ text "This event is worth "
                , b [] [ text <| toString event.points, text " points" ]
                ]

        section =
            event.section
                |> Maybe.map
                    (\section ->
                        [ p [] [ text "This event is for the " ++ section ++ " section" ] ]
                    )

        rsvpIssue issue =
            [ p [ class "has-text-grey-light is-italic" ] [ text issue ] ]

        requestAbsence =
            -- TODO: why do we need rsvpIssue
            event.rsvpIssue
                |> Maybe.Extra.filter (Posix.now < event.releaseTime)
                |> Maybe.map
                    (\_ ->
                        a
                            [ class "button is-primary is-outlined"
                            , Route.href Route.Event Route.EventTab.TabAbsenceRequest
                            ]
                            [ text "Request Absence" ]
                    )

        attendanceBlock =
            div []
                [ if Posix.Now < event.releaseTime then
                    span [] <|
                        case event.rsvpIssue of
                            Just issue ->
                                rsvpIssue issue

                            Nothing ->
                                rsvpActions event

                  else
                    attendance event
                ]
    in
    div [] List.concat <|
        [ [ title, subtitle ]
        , Maybe.withDefault [] comments
        , [ attendanceBlock ]
        , Maybe.withDefault [] performAt
        , [ points ]
        , Maybe.withDefault [] section
        , Maybe.withDefault [] <| uniform event.uniform
        ]


rsvpActions : FullEvent -> Html Msg
rsvpActions event =
    case ( event.confirmed, event.shouldAttend ) of
        ( True, True ) ->
            [ p []
                [ text "You're "
                , b [] [ text "confirmed" ]
                , text " to be "
                , b [] [ text "attending" ]
                ]
            , a [ class "button is-primary is-outlined", onClick <| \_ -> RsvpToEvent event False ]
                [ text "oops jk, gotta dip" ]
            ]

        ( True, False ) ->
            [ p [] [ text "The officers know you won't be there" ]
            , a [ class "button is-primary" ] [ text "sike I can come. put me in coach!" ]
            ]

        ( False, True ) ->
            [ p [] [ text "You're coming, right?" ]
            , a [ class "button is-primary is-outlined", onClick <| \_ -> RsvpToEvent event False ]
                [ text "sorry fam, not this time" ]
            , a [ class "button is-primary", onClick <| \_ -> RsvpToEvent event True ]
                [ text "yep, I'll be there" ]
            ]

        ( False, False ) ->
            [ p [] [ text "You're not coming, right?" ]
            , a [ class "button is-primary is-outlined", onClick <| \_ -> RsvpToEvent event False ]
                [ text "akshually I can come. you're welcome" ]
            , a [ class "button is-primary" ] [ text "nah, I'm gonna miss it" ]
            ]


attendance : FullEvent -> Html Msg
attendance event =
    case ( event.didAttend, event.shouldAttend ) of
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
            , text <| toString event.points
            , text " points. "
            , a [ href "mailto:gleeclub_officers@lists.gatech.edu?subject=Attendance%20Issue" ]
                [ text "Email the officers" ]
            , text " if you think that's not right."
            ]

        ( False, False ) ->
            [ text "You "
            , b [] [ "weren't there" ]
            , text ", but that's "
            , b [] [ text "ok" ]
            , text "."
            ]


uniform : Maybe Uniform -> Html Msg
uniform eventUniform =
    eventUniform
        |> Maybe.map
            (\uniform ->
                [ p []
                    [ span [] [ text uniform.name ]
                    , span
                        [ style "cursor" "pointer"
                        , class "icon tooltip is-tooltip-multiline has-text-grey-light is-small"
                        , data - tooltip uniform.description
                        ]
                        [ i [ class "far fa-question-circle" ] [] ]
                    , br
                    ]
                ]
            )
