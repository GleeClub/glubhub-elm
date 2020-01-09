module Page.Events.EditCarpools exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, article, button, div, i, section, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (find, uncons)
import Models.Event exposing (Event, EventAttendee, EventCarpool, Member, SimpleAttendance, UpdatedCarpool, eventAttendeeDecoder, eventCarpoolDecoder, eventDecoder)
import Route
import Task
import Utils exposing (Common, RemoteData(..), alert, fullName, getRequest, mapLoaded, postRequest, resultToRemote)



---- MODEL ----


type alias CarpoolData =
    { event : Event
    , attendance : List EventAttendee
    , carpools : List UpdatedCarpool
    }


type MemberSelection
    = UnassignedMembers (List Member) -- covers nobody selected, too
    | Driver UpdatedCarpool
    | Passengers UpdatedCarpool (List Member)


type alias Model =
    { common : Common
    , data : RemoteData CarpoolData
    , selection : MemberSelection
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { common = common, data = Loading, selection = UnassignedMembers [] }, loadData common eventId )



---- UPDATE ----


type Msg
    = OnLoadData (GreaseResult CarpoolData)
    | ClickUnassignedMember Member
    | ClickEmptyUnassignedMemberList
    | ClickDriver UpdatedCarpool
    | ClickPassenger UpdatedCarpool Member
    | ClickEmptyPassengerList UpdatedCarpool
    | AddNewCarpool
    | SaveCarpools
    | OnSaveCarpools (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadData result ->
            ( { model | data = resultToRemote result }, Cmd.none )

        SaveCarpools ->
            case model.data of
                Loaded data ->
                    ( model, updateCarpools model.common data )

                _ ->
                    ( model, Cmd.none )

        OnSaveCarpools (Ok _) ->
            case model.data of
                Loaded data ->
                    ( model
                    , Route.loadPage <|
                        Route.Events
                            { id = Just data.event.id, tab = Just Route.EventCarpools }
                    )

                _ ->
                    ( model, Cmd.none )

        OnSaveCarpools (Err _) ->
            -- TODO: Display an actual error.
            ( model, alert "We failed to update the carpools. Please try again." )

        ClickUnassignedMember member ->
            case model.selection of
                Driver carpool ->
                    ( unassignCarpoolDriver model carpool, Cmd.none )

                Passengers carpool passengers ->
                    ( unassignCarpoolPassengers model carpool passengers, Cmd.none )

                UnassignedMembers members ->
                    if members |> List.any (\m -> m.email == member.email) then
                        ( { model | selection = UnassignedMembers (members |> List.filter (\m -> m.email /= member.email)) }, Cmd.none )

                    else
                        ( { model | selection = UnassignedMembers (members ++ [ member ]) }, Cmd.none )

        ClickEmptyUnassignedMemberList ->
            case model.selection of
                Driver carpool ->
                    ( unassignCarpoolDriver model carpool, Cmd.none )

                Passengers carpool passengers ->
                    ( unassignCarpoolPassengers model carpool passengers, Cmd.none )

                UnassignedMembers _ ->
                    ( model, Cmd.none )

        ClickDriver carpool ->
            case model.selection of
                Driver otherCarpool ->
                    let
                        carpoolMapper c =
                            if c.driver.email == carpool.driver.email then
                                { c | driver = otherCarpool.driver }

                            else if c.driver.email == otherCarpool.driver.email then
                                { c | driver = carpool.driver }

                            else
                                c
                    in
                    ( updateCarpoolsAndClearSelection model (List.map carpoolMapper), Cmd.none )

                Passengers otherCarpool passengers ->
                    if otherCarpool.driver.email == carpool.driver.email then
                        ( model, Cmd.none )

                    else
                        case passengers |> uncons of
                            Just ( firstPassenger, [] ) ->
                                let
                                    carpoolMapper c =
                                        if c.driver.email == carpool.driver.email then
                                            { c
                                                | driver = firstPassenger
                                                , passengers =
                                                    c.passengers
                                                        |> List.filter (\p -> p.email /= firstPassenger.email)
                                            }

                                        else
                                            c
                                in
                                ( updateCarpoolsAndClearSelection model (List.map carpoolMapper), Cmd.none )

                            _ ->
                                ( model, Cmd.none )

                UnassignedMembers members ->
                    if List.isEmpty members then
                        ( { model | selection = Driver carpool }, Cmd.none )

                    else
                        case members |> uncons of
                            Just ( firstMember, [] ) ->
                                let
                                    carpoolMapper c =
                                        if c.driver.email == carpool.driver.email then
                                            { c | driver = firstMember }

                                        else
                                            c
                                in
                                ( updateCarpoolsAndClearSelection model (List.map carpoolMapper), Cmd.none )

                            _ ->
                                ( model, Cmd.none )

        ClickPassenger carpool passenger ->
            case model.selection of
                Driver otherCarpool ->
                    if List.isEmpty otherCarpool.passengers then
                        ( updateCarpoolsAndClearSelection model
                            (List.concatMap
                                (\c ->
                                    if c.driver.email == otherCarpool.driver.email then
                                        []

                                    else if c.driver.email == carpool.driver.email then
                                        [ { c | passengers = c.passengers ++ [ otherCarpool.driver ] } ]

                                    else
                                        [ c ]
                                )
                            )
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Passengers otherCarpool passengers ->
                    if otherCarpool.driver.email == carpool.driver.email then
                        if passengers |> List.any (\p -> p.email == passenger.email) then
                            ( { model | selection = Passengers otherCarpool (passengers |> List.filter (\p -> p.email /= passenger.email)) }, Cmd.none )

                        else
                            ( { model | selection = Passengers otherCarpool (passengers ++ [ passenger ]) }, Cmd.none )

                    else
                        ( updateCarpoolsAndClearSelection model
                            (List.map
                                (\c ->
                                    if c.driver.email == carpool.driver.email then
                                        { c | passengers = c.passengers ++ passengers }

                                    else if c.driver.email == otherCarpool.driver.email then
                                        { c | passengers = c.passengers |> List.filter (\p -> passengers |> List.all (\cp -> cp.email /= p.email)) }

                                    else
                                        c
                                )
                            )
                        , Cmd.none
                        )

                UnassignedMembers members ->
                    if List.isEmpty members then
                        ( { model | selection = Passengers carpool [ passenger ] }, Cmd.none )

                    else
                        ( updateCarpoolsAndClearSelection model
                            (List.map
                                (\c ->
                                    if c.driver.email == carpool.driver.email then
                                        { c | passengers = c.passengers ++ members }

                                    else
                                        c
                                )
                            )
                        , Cmd.none
                        )

        ClickEmptyPassengerList carpool ->
            case model.selection of
                Driver otherCarpool ->
                    if List.isEmpty otherCarpool.passengers then
                        ( updateCarpoolsAndClearSelection model
                            (List.concatMap
                                (\c ->
                                    if c.driver.email == otherCarpool.driver.email then
                                        []

                                    else if c.driver.email == carpool.driver.email then
                                        [ { c | passengers = c.passengers ++ [ otherCarpool.driver ] } ]

                                    else
                                        [ c ]
                                )
                            )
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Passengers otherCarpool passengers ->
                    ( updateCarpoolsAndClearSelection model
                        (List.map
                            (\c ->
                                if c.driver.email == carpool.driver.email then
                                    { c | passengers = c.passengers ++ passengers }

                                else if c.driver.email == otherCarpool.driver.email then
                                    { c | passengers = c.passengers |> List.filter (\p -> passengers |> List.all (\cp -> cp.email /= p.email)) }

                                else
                                    c
                            )
                        )
                    , Cmd.none
                    )

                UnassignedMembers members ->
                    ( updateCarpoolsAndClearSelection model
                        (List.map
                            (\c ->
                                if c.driver.email == carpool.driver.email then
                                    { c | passengers = c.passengers ++ members }

                                else
                                    c
                            )
                        )
                    , Cmd.none
                    )

        AddNewCarpool ->
            ( addNewCarpool model, Cmd.none )


unassignCarpoolPassengers : Model -> UpdatedCarpool -> List Member -> Model
unassignCarpoolPassengers model carpool passengers =
    let
        mapCarpool givenCarpool =
            if givenCarpool.driver.email == carpool.driver.email then
                { givenCarpool | passengers = givenCarpool.passengers |> filterPassengers }

            else
                givenCarpool

        filterPassengers =
            List.filter (\p -> passengers |> List.all (\cp -> cp.email /= p.email))
    in
    updateCarpoolsAndClearSelection model (List.map mapCarpool)


unassignCarpoolDriver : Model -> UpdatedCarpool -> Model
unassignCarpoolDriver model carpool =
    if List.isEmpty carpool.passengers then
        let
            mapCarpools =
                List.filter (\c -> c.driver.email /= carpool.driver.email)
        in
        updateCarpoolsAndClearSelection model mapCarpools

    else
        model


updateCarpoolsAndClearSelection : Model -> (List UpdatedCarpool -> List UpdatedCarpool) -> Model
updateCarpoolsAndClearSelection model carpoolsMapper =
    let
        mapData =
            mapLoaded
                (\data ->
                    { data
                        | carpools = data.carpools |> carpoolsMapper
                    }
                )
    in
    { model
        | selection = UnassignedMembers []
        , data = model.data |> mapData
    }


addNewCarpool : Model -> Model
addNewCarpool model =
    case model.selection of
        UnassignedMembers members ->
            case members |> uncons of
                Just ( firstMember, [] ) ->
                    updateCarpoolsAndClearSelection model
                        (\carpools ->
                            carpools
                                ++ [ { id = Nothing
                                     , driver = firstMember
                                     , passengers = []
                                     }
                                   ]
                        )

                _ ->
                    model

        Passengers carpool passengers ->
            case passengers |> uncons of
                Just ( firstPassenger, [] ) ->
                    updateCarpoolsAndClearSelection model
                        (\carpools ->
                            (carpools
                                |> List.map
                                    (\c ->
                                        if c.driver.email == carpool.driver.email then
                                            { c | passengers = c.passengers |> List.filter (\p -> p.email /= firstPassenger.email) }

                                        else
                                            c
                                    )
                            )
                                ++ [ { id = Nothing
                                     , driver = firstPassenger
                                     , passengers = []
                                     }
                                   ]
                        )

                _ ->
                    model

        Driver carpool ->
            model



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


updateCarpools : Common -> CarpoolData -> Cmd Msg
updateCarpools common data =
    let
        url =
            "/events/" ++ String.fromInt data.event.id ++ "/carpools"
    in
    postRequest common url (data.carpools |> Encode.list serializeCarpool)
        |> Task.attempt OnSaveCarpools


serializeCarpool : UpdatedCarpool -> Encode.Value
serializeCarpool carpool =
    Encode.object
        [ ( "id", carpool.id |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
        , ( "driver", Encode.string carpool.driver.email )
        , ( "passengers", carpool.passengers |> Encode.list (\p -> Encode.string p.email) )
        ]


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
                div [ onClick ClickEmptyUnassignedMemberList ]
                    [ i [] [ text "That's everyone!" ] ]

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
                                    , onClick = ClickUnassignedMember
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
                [ Basics.box
                    [ div [ style "width" "100%", style "padding-bottom" "10px" ]
                        [ Basics.linkButton "Cancel" <|
                            Route.Events { id = Just data.event.id, tab = Nothing }
                        , button
                            [ class "button is-pulled-right is-primary"
                            , onClick SaveCarpools
                            ]
                            [ text "Update Carpools" ]
                        ]
                    , table [ class "table", style "width" "100%" ]
                        (data.carpools |> List.concatMap (carpoolPartialTable model data))
                    , button [ class "button is-fullwidth", onClick AddNewCarpool ]
                        [ text "Pick a driver and then click here to add new carpool" ]
                    ]
                ]
            ]
        ]


carpoolPartialTable : Model -> CarpoolData -> UpdatedCarpool -> List (Html Msg)
carpoolPartialTable model data carpool =
    let
        passengerRows =
            if List.isEmpty carpool.passengers then
                [ tr []
                    [ td [ colspan 5, style "width" "100%", onClick <| ClickEmptyPassengerList carpool ]
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
                                , onClick = ClickPassenger carpool
                                }
                        )
    in
    [ thead []
        [ carpoolMemberRow
            { member = carpool.driver
            , common = model.common
            , event = data.event
            , attendance = findAttendance data carpool.driver.email
            , isDriver = True
            , includeIcon = True
            , selection = model.selection
            , onClick = \_ -> ClickDriver carpool
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
    , selection : MemberSelection
    , onClick : Member -> Msg
    }


carpoolMemberRow : CarpoolMemberRow -> Html Msg
carpoolMemberRow memberRow =
    let
        col =
            if memberRow.isDriver then
                th []

            else
                td [ style "border-bottom-width" "0px" ]

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

        selection =
            case memberRow.selection of
                UnassignedMembers members ->
                    members

                Driver carpool ->
                    [ carpool.driver ]

                Passengers _ passengers ->
                    passengers

        passengerCount =
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
            if selection |> List.any (\member -> member.email == memberRow.member.email) then
                "is-selected"

            else
                ""
        ]
        [ col
            [ if memberRow.includeIcon then
                icon

              else
                text ""
            ]
        , col [ text (memberRow.member |> fullName) ]
        , col [ text memberRow.member.location ]
        , col [ text passengerCount ]
        , col [ Basics.attendanceIcon memberRow.common memberRow.event memberRow.attendance ]
        ]
