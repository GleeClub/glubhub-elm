module Page.Events.EditCarpools exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Html exposing (Html, b, br, div, i, li, section, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Models.Event exposing (Event, EventCarpool, Member, eventCarpoolDecoder, eventDecoder)
import Route exposing (Route)
import Task
import Utils exposing (Common, RemoteData(..), apiUrl, getRequest, handleJsonResponse, spinner)



---- MODEL ----


type alias Model =
    { common : Common
    , eventAndCarpools : RemoteData ( Event, List EventCarpool )
    , selection : List String
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { common = common, eventAndCarpools = Loading, selection = ["sammohr97@gmail.com"] }, loadEventAndCarpools common eventId )



---- UPDATE ----


type Msg
    = OnLoadEventAndCarpools (Result Http.Error ( Event, List EventCarpool ))
    | ClickMember Member
    | ClickEmptyMemberList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadEventAndCarpools (Ok eventAndCarpools) ->
            ( { model | eventAndCarpools = Loaded eventAndCarpools }, Cmd.none )

        OnLoadEventAndCarpools (Err _) ->
            ( { model | eventAndCarpools = Failure }, Cmd.none )

        ClickMember member ->
            ( model, Cmd.none )

        ClickEmptyMemberList ->
            ( model, Cmd.none )



---- DATA ----


loadEventAndCarpools : Common -> Int -> Cmd Msg
loadEventAndCarpools common eventId =
    let
        eventUrl =
            "/events/" ++ String.fromInt eventId

        carpoolUrl =
            eventUrl ++ "/carpools"

        getTask url resolver =
            Http.task
                { method = "GET"
                , url = apiUrl ++ url
                , body = Http.emptyBody
                , headers = [ Http.header "token" common.token ]
                , resolver = resolver
                , timeout = Nothing
                }

        getEvent =
            getTask eventUrl (Http.stringResolver <| handleJsonResponse eventDecoder)

        getCarpools =
            getTask carpoolUrl (Http.stringResolver <| handleJsonResponse <| Decode.list eventCarpoolDecoder)
    in
    Task.attempt OnLoadEventAndCarpools <|
        Task.map2 Tuple.pair getEvent getCarpools


remainingMembers : List EventCarpool -> List Member -> List Member
remainingMembers carpools members =
    let
        carpoolEmails =
            carpools
                |> List.concatMap
                    (\carpool ->
                        carpool.driver.email :: (carpool.passengers |> List.map .email)
                    )
    in
    members |> List.filter (\member -> carpoolEmails |> List.all ((/=) member.email))



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        content =
            case model.eventAndCarpools of
                NotAsked ->
                    [ text "" ]

                Loading ->
                    [ spinner ]

                Loaded ( event, carpools ) ->
                    memberListAndCarpools model.selection model.common.members event carpools

                Failure ->
                    [ text "whoops" ]
    in
    div [ id "carpools" ]
        [ div []
            [ section [ class "section" ]
                [ div [ class "container" ] content ]
            ]
        ]


memberListAndCarpools : List String -> List Member -> Event -> List EventCarpool -> List (Html Msg)
memberListAndCarpools selection members event carpools =
    let
        membersLeft =
            members |> remainingMembers carpools

        remainingMemberTable =
            if List.isEmpty membersLeft then
                div [ onClick ClickEmptyMemberList ] [ i [] [ text "add members here, doof" ] ]

            else
                table [ class "table" ]
                    (membersLeft
                        |> List.map
                            (\member ->
                                carpoolMemberRow
                                    { member = member
                                    , isDriver = False
                                    , includeIcon = False
                                    , selection = selection
                                    , onClick = ClickMember
                                    }
                            )
                    )
    in
    [ Basics.title <| "Carpools for " ++ event.name
    , Basics.columns
        [ Basics.column
            [ Basics.box
                [ remainingMemberTable ]
            ]
        , Basics.column
            [ Basics.box (carpools |> List.map (carpoolTable selection)) ]
        ]
    ]


carpoolTable : List String -> EventCarpool -> Html Msg
carpoolTable selection carpool =
    table [ class "table" ]
        [ thead []
            [ carpoolMemberRow
                { member = carpool.driver
                , isDriver = True
                , includeIcon = True
                , selection = selection
                , onClick = ClickMember
                }
            ]
        , tbody []
            (carpool.passengers
                |> List.map
                    (\passenger ->
                        carpoolMemberRow
                            { member = passenger
                            , isDriver = False
                            , includeIcon = True
                            , selection = selection
                            , onClick = ClickMember
                            }
                    )
            )
        ]


type alias MemberRow =
    { member : Member
    , isDriver : Bool
    , includeIcon : Bool
    , selection : List String
    , onClick : Member -> Msg
    }


carpoolMemberRow : MemberRow -> Html Msg
carpoolMemberRow memberRow =
    let
        col width =
            (if memberRow.isDriver then
                th

             else
                td
            )
                [ style "width" width ]

        icon =
            span [ class "icon" ]
                [ i
                    [ class <|
                        "fas "
                            ++ (if memberRow.isDriver then
                                    "fa-user"

                                else
                                    "fa-users"
                               )
                    ]
                    []
                ]

        passengers =
            if memberRow.member.passengers == 0 then
                ""

            else
                String.fromInt memberRow.member.passengers
    in
    tr
        [ onClick <| memberRow.onClick memberRow.member
        , class <|
            if memberRow.selection |> List.any (\email -> email == memberRow.member.email) then
                "is-selected"

            else
                ""
        ]
        [ col "10%"
            [ if memberRow.includeIcon then
                icon

              else
                text ""
            ]
        , col "40%" [ text memberRow.member.fullName ]
        , col "30%" [ text memberRow.member.location ]
        , col "10%" [ text passengers ]
        , col "10%" [ text passengers ]
        ]
