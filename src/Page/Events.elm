module Page.Events exposing (FullEventTab(..), Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.SelectableList exposing (selectableList)
import Datetime exposing (simpleDateFormatter)
import Error exposing (GreaseResult)
import Html exposing (Html, a, div, li, section, td, text, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (find)
import Maybe.Extra exposing (filter, isJust)
import Models.Event exposing (Event, eventDecoder)
import Page.Events.Attendance as Attendance
import Page.Events.Attendees as Attendees
import Page.Events.Carpools as Carpools
import Page.Events.Details as Details
import Page.Events.EditEvent as EditEvent
import Page.Events.RequestAbsence exposing (requestAbsence)
import Page.Events.Setlist as Setlist
import Route exposing (EventRoute, EventTab(..))
import Task
import Utils exposing (Common, RemoteData(..), alert, eventIsOver, getRequest, mapLoaded, permittedTo, postRequest)



---- MODEL ----


type alias Model =
    { common : Common
    , events : RemoteData (List Event)
    , selected : RemoteData ( Event, FullEventTab )
    }


type FullEventTab
    = FullEventDetails Details.Model
    | FullEventAttendees Attendees.Model
    | FullEventAttendance Attendance.Model
    | FullEventSetlist Setlist.Model
    | FullEventCarpools Carpools.Model
    | FullEventRequestAbsence String
    | FullEventEdit EditEvent.Model


init : Common -> EventRoute -> ( Model, Cmd Msg )
init common route =
    let
        ( toLoadEvent, selected ) =
            case route.id of
                Just eventId ->
                    ( [ loadEvent common eventId (route.tab |> Maybe.withDefault EventDetails) ], Loading )

                Nothing ->
                    ( [], NotAsked )
    in
    ( { common = common
      , events = Loading
      , selected = selected
      }
    , Cmd.batch <| loadEvents common route :: toLoadEvent
    )



---- UPDATE ----


type Msg
    = OnLoadEvents (GreaseResult ( List Event, EventRoute ))
    | SelectEvent Event
    | OnLoadEvent (GreaseResult ( Event, EventTab ))
    | UnselectEvent
    | OnDeleteEvent Int
    | OnEditEvent Event
    | RequestAbsence
    | UpdateAbsenceRequest String
    | OnRequestAbsence (GreaseResult ())
    | ChangeTab EventTab
    | DetailsMsg Details.InternalMsg
    | SetlistMsg Setlist.Msg
    | CarpoolsMsg Carpools.Msg
    | AttendeesMsg Attendees.Msg
    | AttendanceMsg Attendance.Msg
    | EditEventMsg EditEvent.InternalMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.selected ) of
        ( OnLoadEvents (Ok ( allEvents, route )), _ ) ->
            let
                newModel =
                    { model | events = Loaded (List.reverse allEvents) }
            in
            case route.id |> Maybe.andThen (\id -> allEvents |> find (\e -> e.id == id)) of
                Just event ->
                    changeTab event (route.tab |> Maybe.withDefault EventDetails) newModel

                Nothing ->
                    ( newModel, Cmd.none )

        ( OnLoadEvents (Err error), _ ) ->
            ( { model | events = Failure error }, Cmd.none )

        ( UnselectEvent, _ ) ->
            ( { model | selected = NotAsked }, Route.replaceUrl model.common.key <| Route.Events { id = Nothing, tab = Nothing } )

        ( SelectEvent event, _ ) ->
            changeTab event EventDetails model

        ( OnLoadEvent (Ok ( event, tab )), _ ) ->
            changeTab event tab model

        ( OnLoadEvent (Err error), _ ) ->
            ( { model | selected = Failure error }, Cmd.none )

        ( ChangeTab tab, _ ) ->
            case model.selected of
                Loaded ( event, _ ) ->
                    changeTab event tab model

                _ ->
                    ( model, Cmd.none )

        ( RequestAbsence, Loaded ( event, FullEventRequestAbsence reason ) ) ->
            ( model, submitAbsenceRequest model.common reason event.id )

        ( RequestAbsence, _ ) ->
            ( model, Cmd.none )

        ( OnRequestAbsence (Ok _), _ ) ->
            case model.selected of
                Loaded ( event, _ ) ->
                    let
                        ( newModel, newCmd ) =
                            changeTab event EventDetails model
                    in
                    ( newModel
                    , Cmd.batch
                        [ newCmd
                        , alert "Your request has been submitted. You lazy bum!"
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        ( OnRequestAbsence (Err _), _ ) ->
            ( model, alert "We messed up submitting your request. Please be gentle..." )

        ( OnDeleteEvent eventId, _ ) ->
            ( { model
                | selected = NotAsked
                , events = model.events |> mapLoaded (List.filter (\e -> e.id /= eventId))
              }
            , Cmd.none
            )

        ( OnEditEvent event, _ ) ->
            let
                eventMapper e =
                    if e.id == event.id then
                        event

                    else
                        e

                updatedModel =
                    { model
                        | selected = model.selected |> mapLoaded (Tuple.mapFirst (\_ -> event))
                        , events = model.events |> mapLoaded (List.map eventMapper)
                    }
            in
            changeTab event EventDetails updatedModel

        ( DetailsMsg tabMsg, Loaded ( event, FullEventDetails tabModel ) ) ->
            Details.update tabMsg tabModel |> updateWith event FullEventDetails detailsTranslator model

        ( DetailsMsg _, _ ) ->
            ( model, Cmd.none )

        ( AttendeesMsg tabMsg, Loaded ( event, FullEventAttendees tabModel ) ) ->
            Attendees.update tabMsg tabModel |> updateWith event FullEventAttendees AttendeesMsg model

        ( AttendeesMsg _, _ ) ->
            ( model, Cmd.none )

        ( AttendanceMsg tabMsg, Loaded ( event, FullEventAttendance tabModel ) ) ->
            Attendance.update tabMsg tabModel |> updateWith event FullEventAttendance AttendanceMsg model

        ( AttendanceMsg _, _ ) ->
            ( model, Cmd.none )

        ( SetlistMsg tabMsg, Loaded ( event, FullEventSetlist tabModel ) ) ->
            Setlist.update tabMsg tabModel |> updateWith event FullEventSetlist SetlistMsg model

        ( SetlistMsg _, _ ) ->
            ( model, Cmd.none )

        ( CarpoolsMsg tabMsg, Loaded ( event, FullEventCarpools tabModel ) ) ->
            Carpools.update tabMsg tabModel |> updateWith event FullEventCarpools CarpoolsMsg model

        ( CarpoolsMsg _, _ ) ->
            ( model, Cmd.none )

        ( UpdateAbsenceRequest newReason, Loaded ( event, FullEventRequestAbsence _ ) ) ->
            ( { model | selected = Loaded ( event, FullEventRequestAbsence newReason ) }, Cmd.none )

        ( UpdateAbsenceRequest _, _ ) ->
            ( model, Cmd.none )

        ( EditEventMsg tabMsg, Loaded ( event, FullEventEdit tabModel ) ) ->
            EditEvent.update tabMsg tabModel |> updateWith event FullEventEdit editEventTranslator model

        ( EditEventMsg _, _ ) ->
            ( model, Cmd.none )


detailsTranslator : Details.Translator Msg
detailsTranslator =
    Details.translator
        { onInternalMessage = DetailsMsg
        , onDeleteEvent = OnDeleteEvent
        , onEditEvent = OnEditEvent
        , onSwitchTab = ChangeTab
        }


editEventTranslator : EditEvent.Translator Msg
editEventTranslator =
    EditEvent.translator
        { onInternalMessage = EditEventMsg
        , onPropagateUpdate = OnEditEvent
        }


changeTab : Event -> EventTab -> Model -> ( Model, Cmd Msg )
changeTab event tab model =
    let
        ( newModel, newCmd ) =
            case tab of
                EventDetails ->
                    Details.init model.common event |> updateWith event FullEventDetails detailsTranslator model

                EventAttendees ->
                    Attendees.init model.common event.id |> updateWith event FullEventAttendees AttendeesMsg model

                EventAttendance ->
                    Attendance.init model.common event.id |> updateWith event FullEventAttendance AttendanceMsg model

                EventSetlist ->
                    Setlist.init model.common event.id |> updateWith event FullEventSetlist SetlistMsg model

                EventCarpools ->
                    Carpools.init model.common event.id |> updateWith event FullEventCarpools CarpoolsMsg model

                EventRequestAbsence ->
                    ( { model | selected = Loaded ( event, FullEventRequestAbsence "" ) }, Cmd.none )

                EventEdit ->
                    EditEvent.init model.common event |> updateWith event FullEventEdit editEventTranslator model
    in
    ( newModel
    , Cmd.batch
        [ newCmd
        , Route.replaceUrl model.common.key <|
            Route.Events { id = Just event.id, tab = Just tab }
        ]
    )


updateWith :
    Event
    -> (tabModel -> FullEventTab)
    -> (tabMsg -> Msg)
    -> Model
    -> ( tabModel, Cmd tabMsg )
    -> ( Model, Cmd Msg )
updateWith event toModel toMsg model ( tabModel, subCmd ) =
    ( { model | selected = Loaded ( event, toModel tabModel ) }
    , Cmd.map toMsg subCmd
    )



---- DATA ----


submitAbsenceRequest : Common -> String -> Int -> Cmd Msg
submitAbsenceRequest common reason eventId =
    let
        url =
            "/absence_requests/" ++ String.fromInt eventId

        body =
            Encode.object [ ( "reason", Encode.string reason ) ]
    in
    postRequest common url body
        |> Task.attempt OnRequestAbsence


loadEvent : Common -> Int -> EventTab -> Cmd Msg
loadEvent common eventId tab =
    let
        url =
            "/events/" ++ String.fromInt eventId

        decoder =
            Decode.map2 Tuple.pair
                eventDecoder
                (Decode.succeed tab)
    in
    getRequest common url decoder
        |> Task.attempt OnLoadEvent


loadEvents : Common -> EventRoute -> Cmd Msg
loadEvents common route =
    let
        decoder =
            Decode.map2 Tuple.pair
                (Decode.list <| eventDecoder)
                (Decode.succeed route)
    in
    getRequest common "/events?attendance=true" decoder
        |> Task.attempt OnLoadEvents


type alias EventGroups =
    { volunteer : List Event
    , rehearsal : List Event
    , tutti : List Event
    , ombuds : List Event
    }


organizeEvents : Common -> List Event -> ( EventGroups, EventGroups )
organizeEvents common events =
    let
        ( pastEvents, upcomingEvents ) =
            events
                |> List.reverse
                |> List.partition (\e -> e |> eventIsOver common)

        -- TODO: make less fallible somehow?
        groupEvents eventList =
            { volunteer = eventList |> List.filter (\e -> e.type_ == "Volunteer Gig")
            , rehearsal = eventList |> List.filter (\e -> e.type_ == "Rehearsal" || e.type_ == "Sectional")
            , tutti = eventList |> List.filter (\e -> e.type_ == "Tutti Gig")
            , ombuds = eventList |> List.filter (\e -> e.type_ == "Ombuds" || e.type_ == "Other")
            }
    in
    ( groupEvents pastEvents, groupEvents upcomingEvents )


tabIsActive : FullEventTab -> EventTab -> Bool
tabIsActive currentTab tab =
    case ( currentTab, tab ) of
        ( FullEventDetails _, EventDetails ) ->
            True

        ( FullEventAttendees _, EventAttendees ) ->
            True

        ( FullEventAttendance _, EventAttendance ) ->
            True

        ( FullEventSetlist _, EventSetlist ) ->
            True

        ( FullEventCarpools _, EventCarpools ) ->
            True

        ( FullEventRequestAbsence _, EventRequestAbsence ) ->
            True

        ( FullEventEdit _, EventEdit ) ->
            True

        ( _, _ ) ->
            False


tabTitle : EventTab -> String
tabTitle tab =
    case tab of
        EventDetails ->
            "Details"

        EventAttendees ->
            "Who's Attending"

        EventAttendance ->
            "Attendance"

        EventSetlist ->
            "Set List"

        EventCarpools ->
            "Carpools"

        EventRequestAbsence ->
            "Request Absence"

        EventEdit ->
            "Edit"



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ]
            [ div []
                [ model.events |> Basics.remoteContent (pastAndFutureEventColumns model) ]
            ]
        , eventSidebar model.common model.selected
        ]


eventSidebar : Common -> RemoteData ( Event, FullEventTab ) -> Html Msg
eventSidebar common eventAndTab =
    Basics.sidebar
        { data = eventAndTab
        , close = UnselectEvent
        , render =
            \eventTabPair ->
                div [] <| tabContent common eventTabPair
        }


tabContent : Common -> ( Event, FullEventTab ) -> List (Html Msg)
tabContent common ( event, eventTab ) =
    let
        header =
            [ Buttons.back
                { content = "all events"
                , onClick = UnselectEvent
                }
            , Basics.centeredTitle event.name
            , eventTabs common event eventTab
            ]
    in
    case eventTab of
        FullEventDetails tab ->
            header ++ [ Details.view tab |> Html.map detailsTranslator ]

        FullEventAttendees tab ->
            header ++ [ Attendees.view tab |> Html.map AttendeesMsg ]

        FullEventAttendance tab ->
            header ++ [ Attendance.view tab |> Html.map AttendanceMsg ]

        FullEventSetlist tab ->
            header ++ [ Setlist.view tab |> Html.map SetlistMsg ]

        FullEventCarpools tab ->
            header ++ [ Carpools.view tab |> Html.map CarpoolsMsg ]

        FullEventRequestAbsence reason ->
            [ requestAbsence
                { reason = reason
                , event = event
                , updateReason = UpdateAbsenceRequest
                , submit = RequestAbsence
                , cancel = ChangeTab EventDetails
                }
            ]

        FullEventEdit tab ->
            [ Buttons.back
                { content = "cancel editing"
                , onClick = ChangeTab EventDetails
                }
            , EditEvent.view tab |> Html.map editEventTranslator
            ]


pastAndFutureEventColumns : Model -> List Event -> Html Msg
pastAndFutureEventColumns model events =
    let
        ( pastEvents, upcomingEvents ) =
            events |> organizeEvents model.common
    in
    div []
        [ eventColumns model upcomingEvents
        , Basics.divider "Past"
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


eventColumn : Model -> String -> List Event -> Html Msg
eventColumn model name events =
    let
        isSelected event =
            case model.selected of
                Loaded ( selected, _ ) ->
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
            , onSelect = SelectEvent
            , messageIfEmpty = "No events here, misster."
            }
        ]


eventRow : Model -> Event -> List (Html Msg)
eventRow model event =
    [ td [ style "text-align" "center" ]
        [ Basics.attendanceIcon model.common event ]
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


eventTabs : Common -> Event -> FullEventTab -> Html Msg
eventTabs common event currentTab =
    let
        isGig =
            Maybe.Extra.isJust event.gig

        gigTabs =
            [ EventSetlist, EventCarpools ]

        officerTabs =
            [ EventAttendance ]

        allTabs =
            [ EventDetails, EventAttendees ]
                ++ (if isGig then
                        gigTabs

                    else
                        []
                   )
                ++ (if
                        common.user
                            |> Maybe.map (permittedTo "edit-attendance")
                            |> Maybe.withDefault False
                    then
                        officerTabs

                    else
                        []
                   )
    in
    div [ class "tabs" ]
        [ ul [] (allTabs |> List.map (pageLink currentTab)) ]
