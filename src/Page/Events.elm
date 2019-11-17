module Page.Events exposing (FullEventTab(..), Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.SelectableList exposing (..)
import Html exposing (Html, a, div, h1, i, img, li, p, section, span, table, tbody, td, text, tfoot, thead, tr, ul)
import Html.Attributes exposing (class, href, id, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import List.Extra exposing (find)
import Maybe.Extra exposing (filter, isJust, isNothing)
import Models.Event exposing (FullEvent, fullEventDecoder)
import Page.Events.Attendees as Attendees
import Page.Events.Carpools as Carpools
import Page.Events.Details as Details
import Page.Events.RequestAbsence as RequestAbsence
import Page.Events.Setlist as Setlist
import Route exposing (EventRoute, EventTab(..), Route)
import Time exposing (Posix)
import Utils exposing (Common, RemoteData(..), alert, apiUrl, eventIsOver, getRequest, simpleDateFormatter, spinner)



---- MODEL ----


type alias Model =
    { common : Common
    , events : RemoteData (List FullEvent)
    , selected : RemoteData ( FullEvent, FullEventTab )
    }


type FullEventTab
    = FullEventDetails Details.Model
    | FullEventAttendees Attendees.Model
    | FullEventSetlist Setlist.Model
    | FullEventCarpools Carpools.Model
    | FullEventRequestAbsence RequestAbsence.Model


init : Common -> EventRoute -> ( Model, Cmd Msg )
init common route =
    let
        model =
            { common = common
            , events = Loading
            , selected =
                if isJust route.id then
                    Loading

                else
                    NotAsked
            }

        loadEventCmd =
            case ( route.id, route.tab ) of
                ( Nothing, _ ) ->
                    Cmd.none

                ( Just id, tab ) ->
                    loadEvent common id (tab |> Maybe.withDefault EventDetails)
    in
    ( model, Cmd.batch [ loadEvents common, loadEventCmd ] )



---- UPDATE ----


type Msg
    = OnLoadEvents (Result Http.Error (List FullEvent))
    | OnLoadEvent (Result Http.Error ( FullEvent, EventTab ))
    | SelectEvent Int -- event id
    | UnselectEvent
    | ChangeTab EventTab
    | DetailsMsg Details.Msg
    | SetlistMsg Setlist.Msg
    | CarpoolsMsg Carpools.Msg
    | AttendeesMsg Attendees.Msg
    | RequestAbsenceMsg RequestAbsence.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadEvents (Ok allEvents) ->
            ( { model | events = Loaded (List.reverse allEvents) }, Cmd.none )

        OnLoadEvents (Err error) ->
            ( { model | events = Failure }, Cmd.none )

        OnLoadEvent (Ok ( event, tab )) ->
            changeTab event tab model

        UnselectEvent ->
            ( { model | selected = NotAsked, tab = Nothing }, Route.replaceUrl model.common.key <| Route.Events { id = Nothing, tab = Nothing } )

        SelectEvent eventId ->
            ( model, loadEvent model.common eventId EventDetails )

        ChangeTab tab ->
            case model.eventId of
                Nothing ->
                    ( model, Cmd.none )

                Just id ->
                    changeTab id tab model

        _ ->
            case ( msg, model.tab ) of
                ( DetailsMsg tabMsg, Just (FullEventDetails tabModel) ) ->
                    Details.update tabMsg tabModel |> updateWith FullEventDetails DetailsMsg model

                ( AttendeesMsg tabMsg, Just (FullEventAttendees tabModel) ) ->
                    Attendees.update tabMsg tabModel |> updateWith FullEventAttendees AttendeesMsg model

                ( SetlistMsg tabMsg, Just (FullEventSetlist tabModel) ) ->
                    Setlist.update tabMsg tabModel |> updateWith FullEventSetlist SetlistMsg model

                ( CarpoolsMsg tabMsg, Just (FullEventCarpools tabModel) ) ->
                    Carpools.update tabMsg tabModel |> updateWith FullEventCarpools CarpoolsMsg model

                ( RequestAbsenceMsg tabMsg, Just (FullEventRequestAbsence tabModel) ) ->
                    RequestAbsence.update tabMsg tabModel |> updateWith FullEventRequestAbsence RequestAbsenceMsg model

                ( _, _ ) ->
                    ( model, Cmd.none )


tabTitle : EventTab -> String
tabTitle tab =
    case tab of
        EventDetails ->
            "Details"

        EventAttendees ->
            "Who's Attending"

        EventSetlist ->
            "Set List"

        EventCarpools ->
            "Carpools"

        EventRequestAbsence ->
            "Request Absence"



---- DATA ----


changeTab : FullEvent -> EventTab -> Model -> ( Model, Cmd Msg )
changeTab event tab model =
    let
        ( model, newCmd ) =
            case tab of
                EventDetails ->
                    Details.init model.common event.id |> updateWith FullEventDetails DetailsMsg model

                EventAttendees ->
                    Attendees.init model.common event.id |> updateWith FullEventAttendees AttendeesMsg model

                EventSetlist ->
                    Setlist.init model.common event.id |> updateWith FullEventSetlist SetlistMsg model

                EventCarpools ->
                    Carpools.init model.common event.id |> updateWith FullEventCarpools CarpoolsMsg model

                EventRequestAbsence ->
                    RequestAbsence.init model.common event.id |> updateWith FullEventRequestAbsence RequestAbsenceMsg model
    in
    ( { model | selected = Loaded ( event, tabModel ) }
    , Cmd.batch
        [ newCmd
        , Route.replaceUrl model.common.key <| Route.Events { id = Just event.id, tab = Just tab }
        ]
    )


updateWith : (tabModel -> FullEventTab) -> (tabMsg -> Msg) -> Model -> ( tabModel, Cmd tabMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( tabModel, subCmd ) =
    ( { model | tab = Just <| toModel tabModel }
    , Cmd.map toMsg subCmd
    )


loadEvents : Common -> Cmd Msg
loadEvents common =
    getRequest common "/events?full=true" (Http.expectJson OnLoadEvents (Decode.list <| fullEventDecoder))


loadEvent : Common -> Int -> EventTab -> Cmd Msg
loadEvent common eventId tab =
    let
        url =
            "/events/" ++ String.fromInt eventId
    in
    getRequest common url (Http.expectJson OnLoadEvent fullEventDecoder)


type alias EventGroups =
    { volunteer : List FullEvent
    , rehearsal : List FullEvent
    , tutti : List FullEvent
    , ombuds : List FullEvent
    }


organizeEvents : Posix -> List FullEvent -> ( EventGroups, EventGroups )
organizeEvents now events =
    let
        ( upcomingEvents, pastEvents ) =
            events |> List.partition (\e -> e |> eventIsOver now)

        -- TODO: make less fallible somehow?
        groupEvents eventList =
            { volunteer = eventList |> List.filter (\e -> e.type_ == "Volunteer Gig")
            , rehearsal = eventList |> List.filter (\e -> e.type_ == "Rehearsal" || e.type_ == "Sectional")
            , tutti = eventList |> List.filter (\e -> e.type_ == "Tutti Gig")
            , ombuds = eventList |> List.filter (\e -> e.type_ == "Ombuds" || e.type_ == "Other")
            }
    in
    ( groupEvents pastEvents, groupEvents upcomingEvents )


findSelectedEvent : Model -> Maybe FullEvent
findSelectedEvent model =
    case ( model.events, model.selected ) of
        ( Loaded events, Loaded selectedEvent ) ->
            events |> List.Extra.find (\event -> event.id == selectedEvent.id)

        ( _, _ ) ->
            Nothing


tabIsActive : FullEventTab -> EventTab -> Bool
tabIsActive currentTab tab =
    case ( currentTab, tab ) of
        ( FullEventDetails _, EventDetails ) ->
            True

        ( FullEventAttendees _, EventAttendees ) ->
            True

        ( FullEventSetlist _, EventSetlist ) ->
            True

        ( FullEventCarpools _, EventCarpools ) ->
            True

        ( FullEventRequestAbsence _, EventRequestAbsence ) ->
            True

        ( _, _ ) ->
            False



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        content =
            case model.events of
                NotAsked ->
                    text ""

                Loading ->
                    spinner

                Loaded events ->
                    pastAndFutureEventColumns model events

                Failure ->
                    text "Error"
    in
    div [] <|
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ content ]
            ]
        ]
            ++ eventSidebar model.selected


eventSidebar : RemoteData ( FullEvent, FullEventTab ) -> List (Html Msg)
eventSidebar eventAndTab =
    let
        emptySidebar =
            div [ class "sidenav", style "width" "0%" ] []

        overlay =
            div [ class "transparent-overlay", onClick UnselectEvent ] []

        sidebar =
            div
                [ class "sidenav"
                , style "width" "40%"
                , style "padding" "20px"
                , style "padding-top" "80px"
                ]

        viewTab eventTab =
            case eventTab of
                FullEventDetails tab ->
                    Details.view tab |> Html.map DetailsMsg

                FullEventAttendees tab ->
                    Attendees.view tab |> Html.map AttendeesMsg

                FullEventSetlist tab ->
                    Setlist.view tab |> Html.map SetlistMsg

                FullEventCarpools tab ->
                    Carpools.view tab |> Html.map CarpoolsMsg

                FullEventRequestAbsence tab ->
                    RequestAbsence.view tab |> Html.map RequestAbsenceMsg
    in
    case eventAndTab of
        NotAsked ->
            [ emptySidebar ]

        Loading ->
            [ overlay, sidebar [ spinner ] ]

        Loaded ( event, tab ) ->
            [ overlay, sidebar [ Basics.title event.name, eventTabs event tab, viewTab tab ] ]

        Failure ->
            [ overlay, sidebar [ text "whoops" ] ]


pastAndFutureEventColumns : Model -> List FullEvent -> Html Msg
pastAndFutureEventColumns model events =
    let
        ( pastEvents, upcomingEvents ) =
            organizeEvents model.common.now events
    in
    div [ class "container" ]
        [ Basics.title "Upcoming"
        , eventColumns model upcomingEvents
        , Basics.title "Past"
        , eventColumns model pastEvents
        ]


eventColumns : Model -> EventGroups -> Html Msg
eventColumns model eventGroups =
    Basics.columns
        [ eventColumn model "Volunteer" eventGroups.volunteer
        , eventColumn model "Rehearsal" eventGroups.rehearsal
        , eventColumn model "Tutti" eventGroups.tutti
        , eventColumn model "Ombuds" eventGroups.ombuds
        ]


eventColumn : Model -> String -> List FullEvent -> Html Msg
eventColumn model name events =
    let
        isSelected event =
            case model.selected of
                Loaded selected ->
                    selected.id == event.id

                _ ->
                    False
    in
    div [ class "column is-one-quarter is-centered" ]
        [ Basics.title name
        , selectableList
            { listItems = Loaded events
            , render = eventRow model
            , isSelected = isSelected
            , onSelect = \e -> SelectEvent e.id
            , messageIfEmpty = "No events here, misster."
            }
        ]


eventRow : Model -> FullEvent -> List (Html Msg)
eventRow model event =
    let
        didAttend =
            event.attendance |> Maybe.map .didAttend |> Maybe.withDefault False

        shouldAttend =
            event.attendance |> Maybe.map .shouldAttend |> Maybe.withDefault False

        confirmed =
            event.attendance |> Maybe.map .confirmed |> Maybe.withDefault False

        icon =
            if eventIsOver model.common.now event then
                div
                    [ style "white-space" "nowrap"
                    , class <|
                        if didAttend || not shouldAttend then
                            "has-text-success"

                        else
                            "has-text-danger"
                    ]
                    [ Basics.checkOrCross didAttend ]

            else
                div
                    [ class <|
                        if confirmed then
                            "has-text-success"

                        else
                            "has-text-grey"
                    ]
                    [ Basics.checkOrCross shouldAttend ]
    in
    [ td [ style "text-align" "center" ] [ icon ]
    , td [] [ text <| simpleDateFormatter model.common.timeZone event.callTime ]
    , td [] [ text event.name ]
    ]


pageLink : FullEventTab -> EventTab -> Html Msg
pageLink currentTab tab =
    li
        [ class <|
            if tabIsActive currentTab tab then
                "is-active"

            else
                ""
        ]
        [ a [ onClick <| ChangeTab tab ] [ text <| tabTitle tab ] ]


eventTabs : FullEvent -> FullEventTab -> Html Msg
eventTabs event currentTab =
    let
        isGig =
            Maybe.Extra.isJust event.gig

        gigTabs =
            [ EventSetlist, EventCarpools ]

        allTabs =
            [ EventDetails, EventAttendees ]
                ++ (if isGig then
                        gigTabs

                    else
                        []
                   )
    in
    div [ class "tabs" ]
        [ ul [] (allTabs |> List.map (pageLink currentTab)) ]
