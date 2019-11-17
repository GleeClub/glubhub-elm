module Page.Events.Attendees exposing (Model, Msg(..), init, loadAttendees, organizeAttendees, update, view, viewAttendeeSection, viewAttendeesTable)

import Browser.Navigation as Nav
import Html exposing (Html, a, b, button, div, form, h1, img, input, label, section, span, table, tbody, td, text, tr)
import Html.Attributes exposing (class, colspan, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import List.Extra exposing (groupWhile)
import MD5
import Models.Event exposing (EventAttendee, Member, eventAttendeeDecoder, memberDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), apiUrl, getRequest, notFoundView, setToken, spinner)



---- MODEL ----


type alias Model =
    { attendees : RemoteData (List EventAttendee)
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { attendees = Loading }, loadAttendees common eventId )



---- UPDATE ----


type Msg
    = OnLoadAttendees (Result Http.Error (List EventAttendee))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadAttendees (Ok attendees) ->
            ( { attendees = Loaded attendees }, Cmd.none )

        OnLoadAttendees (Err error) ->
            ( { attendees = Failure }, Cmd.none )



---- DATA ----


loadAttendees : Common -> Int -> Cmd Msg
loadAttendees common eventId =
    let
        url =
            "/events/" ++ String.fromInt eventId ++ "/see_whos_attending"
    in
    getRequest common url (Http.expectJson OnLoadAttendees <| Decode.list eventAttendeeDecoder)


organizeAttendees : List EventAttendee -> List ( String, List EventAttendee )
organizeAttendees attendees =
    let
        sortAttendees =
            List.sortBy (\attendee -> attendee.member.section |> Maybe.withDefault "")

        groupedAttendees =
            attendees
                |> sortAttendees
                |> groupWhile (\attendeeA attendeeB -> attendeeA.member.section == attendeeB.member.section)

        buildSection attendeeSection =
            let
                allInSection =
                    [ Tuple.first attendeeSection ] ++ Tuple.second attendeeSection

                sectionName =
                    (Tuple.first attendeeSection).member.section |> Maybe.withDefault "Unsorted"
            in
            ( sectionName, sortAttendees allInSection )
    in
    groupedAttendees
        |> List.map buildSection
        |> List.sortBy (\section -> Tuple.first section)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        content =
            case model.attendees of
                NotAsked ->
                    text ""

                Loading ->
                    spinner

                Loaded attendees ->
                    viewAttendeesTable attendees

                Failure ->
                    text "error"
    in
    div [ id "attendees" ] [ content ]


viewAttendeesTable : List EventAttendee -> Html Msg
viewAttendeesTable attendees =
    table [ class "table is-fullwidth" ]
        (organizeAttendees attendees |> List.map viewAttendeeSection)


viewAttendeeSection : ( String, List EventAttendee ) -> Html Msg
viewAttendeeSection attendeeSection =
    let
        firstRow =
            tr []
                [ td [ colspan 3 ]
                    [ b [] [ text <| Tuple.first attendeeSection ] ]
                ]

        attendeeClass attendance =
            case ( attendance.shouldAttend, attendance.confirmed ) of
                ( True, True ) ->
                    "has-text-success"

                ( True, False ) ->
                    "has-text-grey-light"

                ( False, False ) ->
                    "is-italic has-text-grey-light"

                ( False, True ) ->
                    ""

        attendeeRow attendee =
            tr [ class <| attendeeClass attendee.attendance ]
                [ td [ Route.href <| Route.Profile attendee.member.email ] [ text attendee.member.fullName ]
                , td []
                    [ text <|
                        if attendee.attendance.shouldAttend then
                            "attending"

                        else
                            "not attending"
                    ]
                , td []
                    [ text <|
                        if attendee.attendance.confirmed then
                            "confirmed"

                        else
                            "unconfirmed"
                    ]
                ]
    in
    tbody [] <| [ firstRow ] ++ (Tuple.second attendeeSection |> List.map attendeeRow)
