module Page.Events.List exposing (checkOrCross, viewEventList, viewEventRow, viewFilter, viewFilters)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Maybe.Extra
import Models.Event exposing (FullEvent)
import Page.Events exposing (Msg)



---- UPDATE ----


type Msg
    = ToggleFilter String
    | SelectEvent FullEvent



---- VIEW ----


viewFilters : List String -> List String -> Html Msg
viewFilters activeFilters allFilters =
    div [ class "buttons" ]
        allFilters
        |> List.map viewFilter


viewFilter : String -> Html Msg
viewFilter filter =
    a
        [ key filter
        , class "button is-rounded is-light"
        , onClick <| \_ -> ToggleFilter filter
        , class <|
            if activeFilters.includes filter then
                "is-primary"

            else
                ""
        ]
        [ text filter ]


viewEventList : Common -> Maybe FullEvent -> List FullEvent -> Html Msg
viewEventList common selectedEvent events =
    if List.length events == 0 then
        p [] [ text "No events yet this semester." ]

    else
        div []
            [ viewFilters common.info.eventTypes
            , table [ class "table is-fullwidth is-hoverable" ]
                [ thead [] []
                , tbody [] events |> List.map (\event -> viewEventRow event selectedEvent)
                , tfoot [] []
                ]
            ]


checkOrCross : Bool -> Html Msg
checkOrCross isCheck =
    span [ class "icon is-medium" ]
        [ i
            [ class "fas fa-lg "
                ++ (if event.didAttend then
                        "fa-check"

                    else
                        "fa-times"
                   )
            ]
        ]


viewEventRow : FullEvent -> Maybe FullEvent -> Html Msg
viewEventRow event selectedEvent =
    let
        isSelected =
            Maybe.Extra.filter (\selected -> selected == event) |> Maybe.Extra.isJust
    in
    tr
        [ key <| toString event.id
        , onClick <| SelectEvent event
        , style "background-color" <|
            if isSelected then
                "#eeeeee"

            else
                ""
        ]
        [ td [] [ text event.name ]
        , td [] [ text <| formatCallTime event.callTime ]
        , td [ style "text-align" "center" ]
            [ if event.releaseTime <= Posix.now then
                div
                    [ style "white-space" "nowrap"
                    , class <|
                        if event.didAttend or not event.shouldAttend then
                            "has-text-success"

                        else
                            "has-text-danger"
                    ]
                    [ checkOrCross event.didAttend ]

              else
                div
                    [ class <|
                        if event.confirmed then
                            "has-text-success"

                        else
                            "has-text-grey"
                    ]
                    [ checkOrCross event.shouldAttend ]
            ]
        ]
