module Page.Events exposing (FullEventTab(..), Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.SelectableList exposing (..)
import Error exposing (GreaseResult)
import Html exposing (Html, a, div, h1, i, img, li, p, section, span, table, tbody, td, text, tfoot, thead, tr, ul)
import Html.Attributes exposing (attribute, class, href, id, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom)
import Json.Encode as Encode
import List.Extra exposing (find)
import Maybe.Extra exposing (filter, isJust, isNothing)
import Models.Event exposing (FullEvent, fullEventDecoder)
import Page.Events.Attendees as Attendees
import Page.Events.Carpools as Carpools
import Page.Events.Details as Details
import Page.Events.RequestAbsence exposing (requestAbsence)
import Page.Events.Setlist as Setlist
import Route exposing (EventRoute, EventTab(..), Route)
import Task
import Time exposing (Posix)
import Utils exposing (Common, RemoteData(..), alert, eventIsOver, getRequest, postRequest, remoteToMaybe, simpleDateFormatter)



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
    | FullEventRequestAbsence String


init : Common -> EventRoute -> ( Model, Cmd Msg )
init common route =
    ( { common = common
      , events = Loading
      , selected = NotAsked
      }
    , loadEvents common route
    )



---- UPDATE ----


type Msg
    = OnLoadEvents (GreaseResult ( List FullEvent, EventRoute ))
    | SelectEvent FullEvent
    | UnselectEvent
    | RequestAbsence
    | UpdateAbsenceRequest String
    | OnRequestAbsence (GreaseResult ())
    | ChangeTab EventTab
    | DetailsMsg Details.Msg
    | SetlistMsg Setlist.Msg
    | CarpoolsMsg Carpools.Msg
    | AttendeesMsg Attendees.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.selected ) of
        ( OnLoadEvents (Ok ( allEvents, route )), _ ) ->
            let
                newModel =
                    { model | events = Loaded (List.reverse allEvents) }
            in
            case route.id |> Maybe.andThen (\id -> allEvents |> List.Extra.find (\e -> e.id == id)) of
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

        ( ChangeTab tab, _ ) ->
            case model.selected of
                Loaded ( event, eventTab ) ->
                    changeTab event tab model

                _ ->
                    ( model, Cmd.none )

        ( RequestAbsence, Loaded ( event, FullEventRequestAbsence reason ) ) ->
            ( model, submitAbsenceRequest model.common reason event.id )

        ( RequestAbsence, _ ) ->
            ( model, Cmd.none )

        ( OnRequestAbsence (Ok _), _ ) ->
            case model.selected of
                Loaded ( event, tab ) ->
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

        ( DetailsMsg tabMsg, Loaded ( event, FullEventDetails tabModel ) ) ->
            Details.update tabMsg tabModel |> updateWith event FullEventDetails DetailsMsg model

        ( DetailsMsg _, _ ) ->
            ( model, Cmd.none )

        ( AttendeesMsg tabMsg, Loaded ( event, FullEventAttendees tabModel ) ) ->
            Attendees.update tabMsg tabModel |> updateWith event FullEventAttendees AttendeesMsg model

        ( AttendeesMsg _, _ ) ->
            ( model, Cmd.none )

        ( SetlistMsg tabMsg, Loaded ( event, FullEventSetlist tabModel ) ) ->
            Setlist.update tabMsg tabModel |> updateWith event FullEventSetlist SetlistMsg model

        ( SetlistMsg _, _ ) ->
            ( model, Cmd.none )

        ( CarpoolsMsg tabMsg, Loaded ( event, FullEventCarpools tabModel ) ) ->
            Carpools.update tabMsg tabModel |> updateWith event FullEventCarpools CarpoolsMsg model

        ( CarpoolsMsg _, _ ) ->
            ( model, Cmd.none )

        ( UpdateAbsenceRequest newReason, Loaded ( event, FullEventRequestAbsence oldReason ) ) ->
            ( { model | selected = Loaded ( event, FullEventRequestAbsence newReason ) }, Cmd.none )

        ( UpdateAbsenceRequest _, _ ) ->
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


changeTab : FullEvent -> EventTab -> Model -> ( Model, Cmd Msg )
changeTab event tab model =
    let
        ( newModel, newCmd ) =
            case tab of
                EventDetails ->
                    Details.init model.common event |> updateWith event FullEventDetails DetailsMsg model

                EventAttendees ->
                    Attendees.init model.common event.id |> updateWith event FullEventAttendees AttendeesMsg model

                EventSetlist ->
                    Setlist.init model.common event.id |> updateWith event FullEventSetlist SetlistMsg model

                EventCarpools ->
                    Carpools.init model.common event.id |> updateWith event FullEventCarpools CarpoolsMsg model

                EventRequestAbsence ->
                    ( { model | selected = Loaded ( event, FullEventRequestAbsence "" ) }, Cmd.none )
    in
    ( newModel
    , Cmd.batch
        [ newCmd
        , Route.replaceUrl model.common.key <|
            Route.Events { id = Just event.id, tab = Just tab }
        ]
    )


updateWith :
    FullEvent
    -> (tabModel -> FullEventTab)
    -> (tabMsg -> Msg)
    -> Model
    -> ( tabModel, Cmd tabMsg )
    -> ( Model, Cmd Msg )
updateWith event toModel toMsg model ( tabModel, subCmd ) =
    ( { model | selected = Loaded ( event, toModel tabModel ) }
    , Cmd.map toMsg subCmd
    )


loadEvents : Common -> EventRoute -> Cmd Msg
loadEvents common route =
    getRequest common
        "/events?full=true"
        (Decode.map2 Tuple.pair
            (Decode.list <| fullEventDecoder)
            (Decode.succeed route)
        )
        |> Task.attempt OnLoadEvents


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
    div []
        [ section [ class "section" ]
            [ div []
                [ model.events |> Basics.remoteContent (pastAndFutureEventColumns model) ]
            ]
        , eventSidebar model.selected
        ]


eventSidebar : RemoteData ( FullEvent, FullEventTab ) -> Html Msg
eventSidebar eventAndTab =
    Basics.sidebar
        { data = eventAndTab
        , close = UnselectEvent
        , render =
            \eventTabPair ->
                div [] <| tabContent eventTabPair
        }


tabContent : ( FullEvent, FullEventTab ) -> List (Html Msg)
tabContent ( event, eventTab ) =
    let
        header =
            [ Basics.title event.name
            , eventTabs event eventTab
            ]
    in
    case eventTab of
        FullEventDetails tab ->
            header ++ [ Details.view tab |> Html.map DetailsMsg ] ++ [ absenceRequestButton event ]

        FullEventAttendees tab ->
            header ++ [ Attendees.view tab |> Html.map AttendeesMsg ]

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


pastAndFutureEventColumns : Model -> List FullEvent -> Html Msg
pastAndFutureEventColumns model events =
    let
        ( pastEvents, upcomingEvents ) =
            organizeEvents model.common.now events
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


eventColumn : Model -> String -> List FullEvent -> Html Msg
eventColumn model name events =
    let
        isSelected event =
            model.selected
                |> remoteToMaybe
                |> Maybe.map (\( selected, _ ) -> selected.id == event.id)
                |> Maybe.withDefault False
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


eventRow : Model -> FullEvent -> List (Html Msg)
eventRow model event =
    [ td [ style "text-align" "center" ]
        [ Basics.attendanceIcon model.common event event.attendance ]
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


absenceRequestButton : FullEvent -> Html Msg
absenceRequestButton event =
    -- TODO: why do we need rsvpIssue
    a
        [ class "button is-primary is-outlined"
        , onClick (ChangeTab EventRequestAbsence)
        ]
        [ text "Request Absence" ]
