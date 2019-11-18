module Page.Events.Attendees exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Html exposing (Html, a, b, br, button, div, form, h1, i, img, input, label, section, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, string)
import List.Extra exposing (groupWhile)
import Models.Event exposing (EventAttendee, Member, eventAttendeeDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), getRequest, spinner)



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


separateAttendees : List EventAttendee -> ( ( List EventAttendee, List EventAttendee ), ( List EventAttendee, List EventAttendee ) )
separateAttendees attendees =
    let
        ( attending, notAttending ) =
            attendees |> List.partition (\attendee -> attendee.attendance.shouldAttend)

        splitByConfirmed =
            List.partition (\attendee -> attendee.attendance.confirmed)
    in
    ( splitByConfirmed attending, splitByConfirmed notAttending )



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
                    attendeeTables attendees

                Failure ->
                    text "error"
    in
    div [ id "attendees" ] [ content ]


attendeeTables : List EventAttendee -> Html Msg
attendeeTables attendees =
    let
        ( attending, notAttending ) =
            separateAttendees attendees
    in
    Basics.column
        [ Basics.title "Attending"
        , attendeeTable attending
        , Basics.title "Not Attending"
        , attendeeTable notAttending
        ]


attendeeTable : ( List EventAttendee, List EventAttendee ) -> Html Msg
attendeeTable ( confirmed, notConfirmed ) =
    table [ class "table is-fullwidth" ]
        [ thead []
            [ tr []
                [ th [] [ text "Confirmed" ]
                , th [] [ text "Not Confirmed" ]
                ]
            ]
        , tbody []
            [ tr []
                [ td [ style "width" "50%" ] <| attendeeNameList confirmed
                , td [ style "width" "50%" ] <| attendeeNameList notConfirmed
                ]
            ]
        ]


attendeeNameList : List EventAttendee -> List (Html Msg)
attendeeNameList attendees =
    attendees
        |> List.map (\attendee -> text attendee.member.fullName)
        |> List.intersperse (br [] [])
