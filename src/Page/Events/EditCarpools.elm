module Page.Events.EditCarpools exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, article, b, br, button, div, i, li, section, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, id, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import List.Extra exposing (find)
import Models.Event exposing (Event, EventAttendee, EventCarpool, Member, SimpleAttendance, UpdatedCarpool, eventAttendeeDecoder, eventCarpoolDecoder, eventDecoder)
import Route exposing (Route)
import Task
import Time exposing (Posix)
import Utils exposing (Common, RemoteData(..), apiUrl, getRequest, handleJsonResponse)



---- MODEL ----


type alias CarpoolData =
    { event : Event
    , attendance : List EventAttendee
    , carpools : List UpdatedCarpool
    }


type alias Model =
    { common : Common
    , data : RemoteData CarpoolData
    , selection : List String
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { common = common, data = Loading, selection = [] }, loadData common eventId )



---- UPDATE ----


type Msg
    = OnLoadData (GreaseResult CarpoolData)
    | ClickMember Member
    | ClickEmptyPassengerList Member
    | AddNewCarpool
    | ClickEmptyMemberList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadData (Ok data) ->
            ( { model | data = Loaded data }, Cmd.none )

        OnLoadData (Err error) ->
            ( { model | data = Failure error }, Cmd.none )

        ClickMember member ->
            ( clickMember model member, Cmd.none )

        ClickEmptyPassengerList driver ->
            ( clickEmptyPassengerList driver model, Cmd.none )

        ClickEmptyMemberList ->
            ( { model | selection = [] }, Cmd.none )

        AddNewCarpool ->
            ( addNewCarpool model, Cmd.none )


addNewCarpool : Model -> Model
addNewCarpool model =
    case model.data of
        Loaded data ->
            case model.selection of
                firstEmail :: [] ->
                    case
                        remainingMembers data.carpools model.common.members
                            |> List.Extra.find (\member -> member.email == firstEmail)
                    of
                        Just newDriver ->
                            let
                                carpools =
                                    data.carpools
                                        ++ [ { id = Nothing, driver = newDriver, passengers = [] } ]
                            in
                            { model | selection = [], data = Loaded { data | carpools = carpools } }

                        Nothing ->
                            model

                _ ->
                    model

        _ ->
            model


clickEmptyPassengerList : Member -> Model -> Model
clickEmptyPassengerList driver model =
    model


clickMember : Model -> Member -> Model
clickMember model member =
    case model.data of
        Loaded data ->
            let
                membersNotInCarpools =
                    remainingMembers data.carpools model.common.members
            in
            if List.isEmpty model.selection then
                -- if the current selection is empty
                { model | selection = [ member.email ] }

            else if model.selection |> List.any ((==) member.email) then
                -- or if they select a currently selected member
                { model | selection = model.selection |> List.filter ((/=) member.email) }

            else if model.selection |> List.all (\email -> membersNotInCarpools |> List.any (\m -> m.email == email)) then
                -- or if the current selection is all unassigned members
                allSelectedAreUnassigned model data member membersNotInCarpools

            else
                -- or if the selected member is a driver
                case
                    model.selection
                        |> List.head
                        |> Maybe.andThen
                            (\email ->
                                data.carpools
                                    |> List.Extra.find (\carpool -> carpool.driver.email == email)
                            )
                of
                    Nothing ->
                        model

                    Just carpool ->
                        if (membersNotInCarpools |> List.any (\m -> m.email == member.email)) && List.isEmpty carpool.passengers then
                            -- and they select the unassigned member list
                            { model | selection = [], data = Loaded { data | carpools = data.carpools |> List.Extra.remove carpool } }

                        else
                            model

        _ ->
            model


allSelectedAreUnassigned : Model -> CarpoolData -> Member -> List Member -> Model
allSelectedAreUnassigned model data member membersNotInCarpools =
    if membersNotInCarpools |> List.any (\m -> m.email == member.email) then
        -- and they select another unassigned member
        { model | selection = member.email :: model.selection }

    else
        -- and they select a carpool
        case data.carpools |> List.Extra.find (memberInCarpool member.email) of
            Nothing ->
                model

            Just carpool ->
                if carpool.driver.email == member.email then
                    -- specifically, the driver
                    case model.selection |> collectSelectedMembers model.common.members of
                        firstSelected :: [] ->
                            -- only update the driver if a single member is selected
                            let
                                updateCarpool c =
                                    if c == carpool then
                                        { c | driver = firstSelected }

                                    else
                                        c
                            in
                            { model | selection = [], data = Loaded { data | carpools = data.carpools |> List.map updateCarpool } }

                        _ ->
                            model

                else
                    -- specifically, the passengers
                    let
                        selectedMembers =
                            collectSelectedMembers model.common.members model.selection

                        updateCarpool c =
                            if c == carpool then
                                { c | passengers = c.passengers ++ selectedMembers }

                            else
                                c
                    in
                    { model | selection = [], data = Loaded { data | carpools = data.carpools |> List.map updateCarpool } }



---- DATA ----


loadData : Common -> Int -> Cmd Msg
loadData common eventId =
    let
        eventUrl =
            "/events/" ++ String.fromInt eventId

        attendanceUrl =
            "/events/" ++ String.fromInt eventId ++ "/see_whos_attending"

        carpoolUrl =
            eventUrl ++ "/carpools"

        getEvent =
            getRequest common eventUrl eventDecoder

        getAttendance =
            getRequest common attendanceUrl (Decode.list eventAttendeeDecoder)

        getCarpools =
            getRequest common carpoolUrl (Decode.list eventCarpoolDecoder)
                |> Task.map cleanupCarpools
    in
    Task.attempt OnLoadData <| Task.map3 CarpoolData getEvent getAttendance getCarpools


cleanupCarpools : List EventCarpool -> List UpdatedCarpool
cleanupCarpools carpools =
    carpools
        |> List.map
            (\carpool ->
                { id = Just carpool.id
                , driver = carpool.driver
                , passengers =
                    carpool.passengers
                        |> List.filter (\passenger -> passenger.email /= carpool.driver.email)
                }
            )


collectSelectedMembers : List Member -> List String -> List Member
collectSelectedMembers members emails =
    emails
        |> List.map (\email -> members |> List.Extra.find (\member -> email == member.email))
        |> List.concatMap (\maybeMember -> maybeMember |> Maybe.map List.singleton |> Maybe.withDefault [])


allMembersAreNotInCarpools : CarpoolData -> List String -> Bool
allMembersAreNotInCarpools data selection =
    not
        (selection
            |> List.any
                (\email -> data.carpools |> List.any (memberInCarpool email))
        )


memberInCarpool : String -> UpdatedCarpool -> Bool
memberInCarpool email carpool =
    (carpool.driver.email == email)
        || (carpool.passengers |> List.any (\passenger -> passenger.email == email))


remainingMembers : List UpdatedCarpool -> List Member -> List Member
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


findAttendance : CarpoolData -> String -> Maybe SimpleAttendance
findAttendance data email =
    data.attendance |> find (\attendance -> attendance.member.email == email) |> Maybe.map .attendance



---- VIEW ----


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ model.data
                |> Basics.remoteContent (memberListAndCarpools model)
            ]
        ]


memberListAndCarpools : Model -> CarpoolData -> Html Msg
memberListAndCarpools model data =
    let
        membersLeft =
            model.common.members |> remainingMembers data.carpools

        remainingMemberTable =
            if List.isEmpty membersLeft then
                div [ onClick ClickEmptyMemberList ] [ i [] [ text "That's everyone!" ] ]

            else
                table [ class "table" ]
                    (membersLeft
                        |> List.map
                            (\member ->
                                carpoolMemberRow
                                    { member = member
                                    , common = model.common
                                    , event = data.event
                                    , attendance = findAttendance data member.email
                                    , isDriver = False
                                    , includeIcon = False
                                    , selection = model.selection
                                    , onClick = ClickMember
                                    }
                            )
                    )
    in
    div []
        [ Basics.title <| "Carpools for " ++ data.event.name
        , Basics.columns
            [ Basics.column
                [ Basics.box
                    [ remainingMemberTable ]
                ]
            , Basics.column
                [ Basics.box <|
                    (data.carpools |> List.map (carpoolTable model data))
                        ++ [ button [ class "button is-fullwidth", onClick AddNewCarpool ] [ text "Pick a driver and then click here to add new carpool" ] ]
                ]
            ]
        ]


carpoolTable : Model -> CarpoolData -> UpdatedCarpool -> Html Msg
carpoolTable model data carpool =
    let
        passengerRows =
            if List.isEmpty carpool.passengers then
                [ tr []
                    [ td [ colspan 5, style "width" "100%", onClick <| ClickEmptyPassengerList carpool.driver ]
                        [ article [ class "message" ]
                            [ div [ class "message-body" ]
                                [ text "It sure is lonely here..." ]
                            ]
                        ]
                    ]
                ]

            else
                carpool.passengers
                    |> List.indexedMap
                        (\index passenger ->
                            carpoolMemberRow
                                { member = passenger
                                , common = model.common
                                , event = data.event
                                , attendance = findAttendance data passenger.email
                                , isDriver = False
                                , includeIcon = index == 0
                                , selection = model.selection
                                , onClick = ClickMember
                                }
                        )
    in
    table [ class "table" ]
        [ thead []
            [ carpoolMemberRow
                { member = carpool.driver
                , common = model.common
                , event = data.event
                , attendance = findAttendance data carpool.driver.email
                , isDriver = True
                , includeIcon = True
                , selection = model.selection
                , onClick = ClickMember
                }
            ]
        , tbody [] passengerRows
        ]


type alias CarpoolMemberRow =
    { member : Member
    , common : Common
    , event : Event
    , attendance : Maybe SimpleAttendance
    , isDriver : Bool
    , includeIcon : Bool
    , selection : List String
    , onClick : Member -> Msg
    }


carpoolMemberRow : CarpoolMemberRow -> Html Msg
carpoolMemberRow memberRow =
    let
        col width =
            if memberRow.isDriver then
                th [ style "min-width" width, style "width" width ]

            else
                td [ style "min-width" width, style "width" width, style "border-bottom-width" "0px" ]

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
        , style "cursor" "pointer"
        , style "width" "100%"
        , style "min-width" "100%"
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
        , col "10%" [ Basics.attendanceIcon memberRow.common memberRow.event memberRow.attendance ]
        ]
