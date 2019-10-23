module Page.Events exposing (Model, Msg(..), init, loadEvents, update, view, viewMemberTable)

import Html exposing (Html, a, div, h1, img, section, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, href, id, src)
import Http
import Json.Decode as Decode
import Models.Event exposing (FullEvent, fullEventDecoder)
import Route exposing (Route, EventTab(..))
import Utils exposing (Common, RemoteData(..), apiUrl, notFoundView, spinner, getRequest, alert, getRequest)
import Page.Events.Details exposing (viewEventDetails)
import Page.Events.List exposing (viewEventList)
import Maybe.Extra exposing (isJust, isNothing, filter)


---- MODEL ----


type alias Model =
    { common : Common
    , events : RemoteData (List FullEvent)
    , event : Maybe FullEvent
    , activeFilters : List String
    , tab : Maybe EventTab
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , events = Loading
      , event = Nothing
      , confirmedEvents = []
      , activeFilters = common.info.eventTypes |> List.map (\type_ -> type_.name)
      , tab = Nothing
      } 
    , loadEvents common
    )



---- UPDATE ----


type Msg
    = OnFetchEvents (Result Http.Error (List FullEvent))
    | ToggleFilter String -- the type of event
    | SelectEvent Int -- event id
    | ChangeTab EventTab
    | Rsvp Int Bool -- event id and whether they plan to attend
    | OnRsvp (Result Http.Error ())
    | InputAbsenceRequestReason String -- the reason for the absence
    | SubmitAbsenceRequest
    | OnSubmitAbsenceRequest (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFetchEvents (Ok allEvents) ->
            ( { model | members = Loaded (List.reverse allEvents) }
            , Cmd.none
            )

        OnFetchEvents (Err error) ->
            ( { model | members = Failure }, Cmd.none )

        ToggleFilter eventType ->
            ( { model | activeFilters = toggleFilter model.activeFilters eventType }, Cmd.none )

        SelectEvent eventId ->
            ( { model | event = Just eventId }, Cmd ChangeTab defaultTabDetails )

        ChangeTab tab ->
            case model.event of
                Just eventId ->
                    ( { model | tab = Just tab }
                    , Route.replaceUrl common.key <| Route.routeToString Route.Event { id = event.id, tab = Just tab }
                    )
                    
                Nothing ->
                    ( model, Cmd.none )

        Rsvp eventId attending ->


        OnRsvp response ->
            onRsvp 
            ( { model | tab = TabDetails true }, rsvp event attending )
            ( { model | tab = TabDetails true } )

        InputAbsenceRequestReason reason ->
            ( {model | tab = TabRequestAbsence reason}, Cmd.none )

        SubmitAbsenceRequest ->
            case ( model.tab, model.event ) of
                ( TabRequestAbsence reason, Just eventId ) ->
                    ( model, submitAbsenceRequest model.common reason eventId )
                
                ( _, _ ) -> 
                    ( model, Cmd.none )

        OnSubmitAbsenceRequest (Ok _) ->
            ( model, Cmd.batch [ ChangeTab TabDetails, alert "Your request was submitted.  You lazy bum!" ] )

        OnSubmitAbsenceRequest (Err _) ->
            ( model, alert "There was an error submitting your request. Please try again." )



---- DATA ----



loadEvents : Common -> Cmd Msg
loadEvents common =
    getRequest common "/events?full=true" (Http.expectJson OnFetchEvents (Decode.list <| fullEventDecoder))


toggleFilter : List String -> String -> List String
toggleFilter activeFilters eventType =
    if activeFilters|> List.any (\type_ -> type_ == eventType) then
        activeFilters <| List.filter \_filter -> filter != eventType
    else
        activeFilters ++ eventType


defaultTabDetails : TabDetails TabDetailsData
defaultTabDetails =
    TabDetails { confirmedAttendance = False, sendingRsvp = False }


filterEvents : List FullEvent -> List String -> List FullEvent
filterEvents events eventTypes = 
  events |> List.filter (\type_ -> (eventTypes |> List.any \type_ -> event.type_ == type_) )


rsvp : Common -> Int -> Bool -> ( Model, Cmd Msg )
rsvp common eventId attending =
    let
        updateEvent event =
            if event.id == eventId then
                { event | shouldAttend = attending }
            else 
                event

        events =
            case model.events of
                Loaded allEvents ->
                    allEvents
            
                _ ->
                    []
                    
        url =
            "/events/" ++ String.fromInt eventId ++ "/rsvp/" ++ if attending then "true" else "false"

        request =
            getRequest common url (Http.expectWhatever OnRsvp)

    in
        ( { model | tab = TabDetails True, events = Loaded <| List.map updateEvent events }
        , request
        )

onRsvp : Common -> Int -> Bool -> Cmd Msg
onRsvp common eventId attending =
    



--   public rsvp(gcEvent: FullEvent, attending: boolean): void {
--     if (attending) {
--       this.common.apiGet(`events/${gcEvent.id}/rsvp/${gcEvent.shouldAttend}`, {}, () => {
--         gcEvent.confirmed = true;
--       }, (error) => {
--         alert(error.message);
--         gcEvent.rsvpIssue = error.message;
--       });
--     } else {
--       gcEvent.shouldAttend = false;
--       this.common.apiGet(`events/${gcEvent.id}/rsvp/${gcEvent.shouldAttend}`, {}, () => {
--       }, (error) => {
--         alert(error.message);
--         gcEvent.rsvpIssue = error.message;
--         gcEvent.shouldAttend = true;
--       });
--     }
--   }


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
                    viewEventList <| filterEvents event

                Failure ->
                    text "Error"
    in  
        div [ id "events" ]
            [ section [ class "section" ]
                [ div [ class "container" ]
                    [ div [ class "columns" ]
                        [ div [ class "column is-half" ]
                            div [ class "box" ] [ content ]
                        ]
                    ]
                ]
            ]
    


pageLink : Model -> EventTab -> String -> Html Msg
pageLink model tab tabName =
    li
        [ class if model.tab == tab then "is-active" else "" ]
        [ a [ onClick ChangeTab tab ] [ text tabName ] ]


eventTabs : Model -> Html Msg
eventTabs model =
    let
        isGig =
            model.event |> Maybe.andThen (\event -> event.gig) |> Maybe.Extra.isJust
                 
        gigTabs =
            [ ( Setlist, "Set List" ), ( Carpools, "Carpools") ]

        tabPairs =
            [ ( Details, "Details" ), ( Attendees, "Who's Attending") ] ++ if isGig then gigTabs else []

    in
        div [ class "tabs" ]
            [ ul [] tabPairs |> (List.map <| \tab, tabName -> pageLink model tab tabName) ]


viewSelectedEvent : Model -> Html Msg
viewSelectedEvent model =
    let 
        tabViewer =
            case model.tab |> Maybe.withDefault Details of
                Details ->
                    viewEventDetails
            
                Attendees ->
                    viewEventAttendees

                Setlist ->
                    viewEventSetlist

                Carpools ->
                    viewEventCarpools

        content =
            case model.selectedEvent of 
                Nothing ->
                    [ p [] [ text "Select an event" ] ]

                Just event ->
                    [ eventTabs model
                    , tabViewer model event
                    ]
    in 
        div [ class "column" ]
            [ div [ id "deetsBox", class "box is-6" ] content ]




