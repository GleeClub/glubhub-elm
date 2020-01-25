module Page.Profile exposing (Model, Msg(..), init, update, view)

import Browser.Navigation exposing (reload)
import Components.Basics as Basics
import Components.DeleteModal exposing (deleteModal)
import Components.Forms exposing (checkboxInput, numberInput)
import Datetime
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, div, h1, i, img, input, li, p, section, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Models.Event
    exposing
        ( ActiveSemester
        , Event
        , GradeChange
        , Member
        , SimpleAttendance
        , activeSemesterDecoder
        , defaultSimpleAttendance
        , eventAttendeeDecoder
        , eventDecoder
        , gradeChangeDecoder
        , memberDecoder
        )
import Models.Info exposing (Enrollment(..), Transaction, enrollmentToString, transactionDecoder)
import Permissions
import Route
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), deleteRequest, fullName, getRequest, isActiveClass, mapLoaded, postRequest, resultToRemote, roundToTwoDigits, setOldToken, setToken)



---- MODEL ----


type alias Model =
    { common : Common
    , member : RemoteData ( Member, ProfileTab )
    , loginAsState : SubmissionState
    , deleteState : Maybe SubmissionState
    }


type ProfileTab
    = ProfileDetails (Maybe ProfileDetailsData)
    | ProfileMoney (RemoteData (List Transaction))
    | ProfileAttendance (RemoteData (List Event))
    | ProfileSemesters (RemoteData (List ActiveSemester))


type alias ProfileDetailsData =
    { form : ProfileForm
    , state : SubmissionState
    }


type alias ProfileForm =
    { firstName : String
    , preferredName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , location : String
    , onCampus : Bool
    , major : String
    , hometown : String
    , passengers : Int
    , enrollment : Maybe Enrollment
    , section : Maybe String
    , about : String
    , picture : String
    , arrivedAtTech : Int
    , gatewayDrug : String
    , conflicts : String
    , dietaryRestrictions : String
    }


formForMember : Member -> ProfileForm
formForMember member =
    { firstName = member.firstName
    , preferredName = member.preferredName |> Maybe.withDefault ""
    , lastName = member.lastName
    , email = member.email
    , phoneNumber = member.phoneNumber
    , location = member.location
    , onCampus = member.onCampus |> Maybe.withDefault False
    , major = member.major |> Maybe.withDefault ""
    , hometown = member.hometown |> Maybe.withDefault ""
    , passengers = member.passengers
    , enrollment = member.enrollment
    , section = member.section
    , about = member.about |> Maybe.withDefault ""
    , picture = member.picture |> Maybe.withDefault ""
    , arrivedAtTech = member.arrivedAtTech |> Maybe.withDefault 1
    , gatewayDrug = member.gatewayDrug |> Maybe.withDefault ""
    , conflicts = member.conflicts |> Maybe.withDefault ""
    , dietaryRestrictions = member.dietaryRestrictions |> Maybe.withDefault ""
    }


init : Common -> String -> ( Model, Cmd Msg )
init common email =
    let
        model =
            { common = common
            , member = Loading
            , loginAsState = NotSentYet
            , deleteState = Nothing
            }
    in
    case common.members |> List.Extra.find (\member -> member.email == email) of
        Just member ->
            ( { model | member = Loaded ( member, ProfileDetails Nothing ) }, Cmd.none )

        Nothing ->
            ( model, loadMember common email )



---- UPDATE ----


type Msg
    = OnLoadMember (GreaseResult Member)
    | Logout
    | LoginAsMember String
    | OnLoginAsMember (GreaseResult String)
    | ChangeDeleteState (Maybe SubmissionState)
    | DeleteMember String
    | OnDeleteMember (GreaseResult ())
    | ChangeTab ProfileTab
    | OnLoadMemberTransactions (GreaseResult (List Transaction))
    | OnLoadSemesters (GreaseResult (List ActiveSemester))
    | OnLoadAttendance (GreaseResult (List Event))
    | UpdateEventAttendance Int SimpleAttendance


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadMember (Ok member) ->
            ( { model | member = Loaded ( member, ProfileDetails Nothing ) }, Cmd.none )

        OnLoadMember (Err error) ->
            ( { model | member = Failure error }, Cmd.none )

        Logout ->
            ( model, Cmd.batch [ setToken Nothing, reload ] )

        LoginAsMember email ->
            ( { model | loginAsState = Sending }, loginAsMember model.common email )

        OnLoginAsMember (Ok newToken) ->
            ( model
            , Cmd.batch
                [ setOldToken <| Just model.common.token
                , setToken <| Just newToken
                , reload
                ]
            )

        OnLoginAsMember (Err error) ->
            ( { model | loginAsState = ErrorSending error }, Cmd.none )

        ChangeDeleteState newState ->
            ( { model | deleteState = newState }, Cmd.none )

        DeleteMember email ->
            ( { model | deleteState = Just Sending }, deleteMember model.common email )

        OnDeleteMember (Ok _) ->
            ( model, Route.loadPage Route.Roster )

        OnDeleteMember (Err error) ->
            ( { model | deleteState = Just <| ErrorSending error }, Cmd.none )

        ChangeTab newTab ->
            case model.member of
                Loaded ( member, _ ) ->
                    ( { model | member = Loaded ( member, newTab ) }
                    , case newTab of
                        ProfileDetails _ ->
                            Cmd.none

                        ProfileMoney _ ->
                            loadMemberTransactions model.common member

                        ProfileSemesters _ ->
                            loadSemesters model.common member

                        ProfileAttendance _ ->
                            loadAttendance model.common member
                    )

                _ ->
                    ( model, Cmd.none )

        OnLoadMemberTransactions result ->
            case model.member of
                Loaded ( member, ProfileMoney _ ) ->
                    ( { model | member = Loaded ( member, ProfileMoney (result |> resultToRemote) ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnLoadSemesters result ->
            case model.member of
                Loaded ( member, ProfileSemesters _ ) ->
                    ( { model | member = Loaded ( member, ProfileSemesters (result |> resultToRemote) ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnLoadAttendance result ->
            case model.member of
                Loaded ( member, ProfileAttendance _ ) ->
                    ( { model | member = Loaded ( member, ProfileAttendance (result |> resultToRemote) ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateEventAttendance eventId attendance ->
            case model.member of
                Loaded ( member, ProfileAttendance events ) ->
                    let
                        eventMapper event =
                            if event.id == eventId then
                                { event | attendance = Just attendance }

                            else
                                event
                    in
                    ( { model
                        | member =
                            Loaded
                                ( member
                                , ProfileAttendance (events |> mapLoaded (List.map eventMapper))
                                )
                      }
                    , updateAttendance model.common eventId member.email attendance
                    )

                _ ->
                    ( model, Cmd.none )



---- DATA ----


loadMember : Common -> String -> Cmd Msg
loadMember common email =
    getRequest common ("/members/" ++ email) memberDecoder |> Task.attempt OnLoadMember


loginAsMember : Common -> String -> Cmd Msg
loginAsMember common email =
    let
        url =
            "/members/" ++ email ++ "/login_as"

        decoder =
            Decode.field "token" Decode.string
    in
    getRequest common url decoder
        |> Task.attempt OnLoginAsMember


deleteMember : Common -> String -> Cmd Msg
deleteMember common email =
    let
        url =
            "/members/" ++ email ++ "?confirm=true"
    in
    deleteRequest common url
        |> Task.attempt OnDeleteMember


loadMemberTransactions : Common -> Member -> Cmd Msg
loadMemberTransactions common member =
    let
        url =
            "/transactions/" ++ member.email
    in
    getRequest common url (Decode.list transactionDecoder)
        |> Task.attempt OnLoadMemberTransactions


loadSemesters : Common -> Member -> Cmd Msg
loadSemesters common member =
    let
        url =
            "/members/" ++ member.email ++ "?details=true"

        decoder =
            Decode.field "semesters" (Decode.list activeSemesterDecoder)
    in
    getRequest common url decoder
        |> Task.attempt OnLoadSemesters


loadAttendance : Common -> Member -> Cmd Msg
loadAttendance common member =
    let
        url =
            "/events?full=true"
    in
    getRequest common url (Decode.list eventDecoder)
        |> Task.attempt OnLoadAttendance


updateAttendance : Common -> Int -> String -> SimpleAttendance -> Cmd Msg
updateAttendance common eventId email attendance =
    let
        url =
            "/events/" ++ String.fromInt eventId ++ "/attendance/" ++ email
    in
    postRequest common url (serializeAttendance attendance)
        |> Task.andThen (\_ -> getRequest common "/events?full=true" (Decode.list eventDecoder))
        |> Task.attempt OnLoadAttendance


serializeAttendance : SimpleAttendance -> Encode.Value
serializeAttendance attendance =
    Encode.object
        [ ( "shouldAttend", Encode.bool attendance.shouldAttend )
        , ( "didAttend", Encode.bool attendance.didAttend )
        , ( "confirmed", Encode.bool attendance.confirmed )
        , ( "minutesLate", Encode.int attendance.minutesLate )
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ model.member
                    |> mapLoaded Tuple.first
                    |> Basics.remoteContent (viewProfile model)
                ]
            ]
        , if
            model.common.user
                |> Maybe.map (.positions >> List.isEmpty)
                |> Maybe.withDefault True
          then
            text ""

          else
            section
                [ class "section" ]
                [ div [ class "container" ]
                    [ Basics.box
                        [ model.member
                            |> Basics.remoteContent (viewProfileTab model)
                        ]
                    ]
                ]
        ]


viewProfile : Model -> Member -> Html Msg
viewProfile model member =
    let
        image =
            div [ class "placeholder" ]
                [ img
                    [ style "background-image" <|
                        "url("
                            ++ (member.picture |> Maybe.withDefault "https://picsum.photos/250")
                            ++ ")"
                    ]
                    []
                ]

        officership =
            if List.isEmpty member.positions then
                "Member"

            else
                String.join ", " member.positions

        majorAndMinor =
            (member.major
                |> Maybe.withDefault "No major"
            )
                ++ (member.minor
                        |> Maybe.map (\minor -> ", minoring in " ++ minor)
                        |> Maybe.withDefault ""
                   )

        rows =
            [ p []
                [ h1
                    [ class "subtitle is-3"
                    , style "margin-bottom" "initial"
                    ]
                    [ text (member |> fullName) ]
                , i [] [ text <| "\"" ++ (member.about |> Maybe.withDefault "no quote") ++ "\"" ]
                ]
            , text officership
            , Basics.emailLink member.email
            , Basics.phoneLink member.phoneNumber
            , text (member.section |> Maybe.withDefault "Homeless")
            , text majorAndMinor
            , div [ class "field is-grouped" ]
                (userActions model member
                    |> List.map (\action -> p [ class "control" ] [ action ])
                )
            , model.deleteState
                |> Maybe.map (deleteMemberModal member)
                |> Maybe.withDefault (text "")
            ]
    in
    Basics.columns
        [ Basics.narrowColumn [ image ]
        , Basics.column (List.intersperse (br [] []) rows)
        ]


userActions : Model -> Member -> List (Html Msg)
userActions model member =
    let
        isCurrentUser =
            model.common.user
                |> Maybe.map (\u -> u.email == member.email)
                |> Maybe.withDefault False

        firstName =
            member.preferredName |> Maybe.withDefault member.firstName
    in
    if isCurrentUser then
        [ a [ class "button", onClick Logout ]
            [ text "Log Out" ]
        , a [ class "button", Route.href Route.EditProfile ]
            [ text "Edit your profile" ]
        ]

    else
        -- TODO: fix dumb import error
        [ Basics.renderIfHasPermission model.common "switch-user" <|
            a [ class "button", onClick <| LoginAsMember member.email ]
                [ text <| "Log in as " ++ firstName ]
        , Basics.renderIfHasPermission model.common "delete-user" <|
            a [ class "button", onClick <| ChangeDeleteState <| Just NotSentYet ]
                [ text <| "Delete " ++ firstName ]
        ]


deleteMemberModal : Member -> SubmissionState -> Html Msg
deleteMemberModal member state =
    deleteModal
        { title = "Delete " ++ (member |> fullName) ++ "?"
        , cancel = ChangeDeleteState Nothing
        , confirm = DeleteMember member.email
        , state = state
        , content =
            div []
                [ text <| "Are you sure you want to delete " ++ (member |> fullName) ++ "?"
                , br [] []
                , i [] [ text "Think of what Uncle Ben would say. No, not the rice one." ]
                ]
        }


viewProfileTab : Model -> ( Member, ProfileTab ) -> Html Msg
viewProfileTab model ( member, tab ) =
    div []
        [ profileTabList tab
        , case tab of
            ProfileDetails currentForm ->
                profileDetailsTab model.common member currentForm

            ProfileMoney data ->
                data |> Basics.remoteContent (profileMoney model.common)

            ProfileSemesters data ->
                data |> Basics.remoteContent profileSemesters

            ProfileAttendance data ->
                data |> Basics.remoteContent (profileAttendance model.common)
        ]


profileTabList : ProfileTab -> Html Msg
profileTabList currentTab =
    let
        allTabs =
            [ ( "Details", ProfileDetails Nothing )
            , ( "Money", ProfileMoney Loading )
            , ( "Attendance", ProfileAttendance Loading )
            , ( "Semesters", ProfileSemesters Loading )
            ]
    in
    div [ class "tabs" ]
        [ ul []
            (allTabs
                |> List.map
                    (\( tabName, tabInit ) ->
                        li [ class (isActiveClass (tabIsActive currentTab tabInit)) ]
                            [ a [ onClick (ChangeTab tabInit) ] [ text tabName ] ]
                    )
            )
        ]


tabIsActive : ProfileTab -> ProfileTab -> Bool
tabIsActive currentTab tab =
    case ( currentTab, tab ) of
        ( ProfileDetails _, ProfileDetails _ ) ->
            True

        ( ProfileMoney _, ProfileMoney _ ) ->
            True

        ( ProfileAttendance _, ProfileAttendance _ ) ->
            True

        ( ProfileSemesters _, ProfileSemesters _ ) ->
            True

        _ ->
            False


profileDetailsTab : Common -> Member -> Maybe ProfileDetailsData -> Html Msg
profileDetailsTab common member currentForm =
    case currentForm of
        Nothing ->
            readOnlyProfileDetails member

        _ ->
            text ""


readOnlyProfileDetails : Member -> Html Msg
readOnlyProfileDetails member =
    let
        rows =
            [ ( "First Name", member.firstName )
            , ( "Preferred Name", member.preferredName |> Maybe.withDefault "" )
            , ( "Last Name", member.lastName )
            , ( "Email", member.email )
            , ( "Phone Number", member.phoneNumber )
            , ( "Location", member.location )
            , ( "On Campus"
              , if member.onCampus |> Maybe.withDefault False then
                    "true"

                else
                    "false"
              )
            , ( "Enrollment"
              , case member.enrollment of
                    Nothing ->
                        "Inactive"

                    Just Class ->
                        "Class"

                    Just Club ->
                        "Club"
              )
            , ( "Section", member.section |> Maybe.withDefault "Homeless" )
            , ( "About", member.about |> Maybe.withDefault "" )
            , ( "Picture", member.picture |> Maybe.withDefault "" )
            , ( "Arrived At Tech", member.arrivedAtTech |> Maybe.map String.fromInt |> Maybe.withDefault "" )
            , ( "Gateway Drug", member.gatewayDrug |> Maybe.withDefault "" )
            , ( "Conflicts", member.conflicts |> Maybe.withDefault "" )
            , ( "Dietary Restrictions", member.dietaryRestrictions |> Maybe.withDefault "" )
            ]
    in
    table [ class "table" ]
        [ tbody []
            (rows
                |> List.map
                    (\( name, value ) ->
                        tr [ class "no-bottom-border" ]
                            [ td [ style "text-align" "right" ] [ b [] [ text name ] ]
                            , td [] [ text value ]
                            ]
                    )
            )
        ]


profileMoney : Common -> List Transaction -> Html Msg
profileMoney common transactions =
    table [ class "table is-striped" ]
        [ tbody []
            (transactions
                |> List.sortBy (.time >> posixToMillis >> (*) -1)
                |> List.map
                    (\transaction ->
                        tr [ class "no-bottom-border" ]
                            (transactionRow common transaction
                                |> List.map (\cell -> td [] [ cell ])
                            )
                    )
            )
        ]


transactionRow : Common -> Transaction -> List (Html Msg)
transactionRow common transaction =
    let
        summary =
            transaction.type_
                ++ (if String.isEmpty transaction.description then
                        ""

                    else
                        " (" ++ transaction.description ++ ")"
                   )

        amount =
            if transaction.amount < 0 then
                span
                    [ style "color" "green" ]
                    [ text <|
                        "("
                            ++ (transaction.amount
                                    |> (*) -1
                                    |> String.fromInt
                               )
                            ++ ")"
                    ]

            else
                text (transaction.amount |> String.fromInt)

        isResolved =
            if transaction.resolved then
                "Resolved"

            else
                "Outstanding"
    in
    [ text (transaction.time |> Datetime.simpleDateWithYearFormatter common.timeZone)
    , text summary
    , amount
    , text isResolved
    ]


profileSemesters : List ActiveSemester -> Html Msg
profileSemesters semesters =
    let
        headerRow =
            tr []
                ([ "Semester", "Status", "Section", "Score" ]
                    |> List.map (\column -> th [] [ text column ])
                )

        semesterRow semester =
            tr [ class "no-bottom-border" ]
                (semesterRowValues semester
                    |> List.map (\cell -> td [] [ text cell ])
                )

        semesterRowValues semester =
            [ semester.semester
            , semester.enrollment
                |> Maybe.map enrollmentToString
                |> Maybe.withDefault "Inactive"
            , semester.section
                |> Maybe.withDefault "Homeless"
            , semester.grades.finalGrade
                |> roundToTwoDigits
                |> String.fromFloat
            ]
    in
    table [ class "table" ]
        [ thead []
            [ headerRow ]
        , tbody []
            (semesters |> List.map semesterRow)
        ]


profileAttendance : Common -> List Event -> Html Msg
profileAttendance common events =
    let
        headerRow =
            tr []
                ([ "Date"
                 , "Event"
                 , "Type"
                 , "Should Attend?"
                 , "Did Attend?"
                 , "Mins Late"
                 , "Point Change"
                 , "PartialScore"
                 , "Rationale"
                 ]
                    |> List.map (\column -> th [] [ text column ])
                )

        finalGrade =
            events
                |> List.filterMap .gradeChange
                |> List.Extra.last
                |> Maybe.map .partialScore
                |> Maybe.withDefault 100.0

        eventRow event =
            tr [ class "no-bottom-border" ]
                (eventRowValues event
                    |> List.map (\cell -> td [] [ cell ])
                )

        eventRowValues event =
            let
                attendance =
                    event.attendance
                        |> Maybe.withDefault { defaultSimpleAttendance | shouldAttend = event.defaultAttend }
            in
            [ text (event.callTime |> Datetime.dateFormatter common.timeZone)
            , a [ Route.href <| Route.Events { id = Just event.id, tab = Nothing } ] [ text event.name ]
            , text event.type_
            , checkboxInput
                { content = ""
                , isChecked = attendance.shouldAttend
                , onChange = \shouldAttend -> UpdateEventAttendance event.id { attendance | shouldAttend = shouldAttend }
                }
            , checkboxInput
                { content = ""
                , isChecked = attendance.didAttend
                , onChange = \didAttend -> UpdateEventAttendance event.id { attendance | didAttend = didAttend }
                }
            , input
                [ class "input"
                , type_ "number"
                , placeholder "0"
                , value (attendance.minutesLate |> String.fromInt)
                , onInput
                    (\minutesLate ->
                        UpdateEventAttendance event.id
                            { attendance
                                | minutesLate =
                                    minutesLate
                                        |> String.toInt
                                        |> Maybe.withDefault 0
                            }
                    )
                ]
                []
            , text
                (event.gradeChange
                    |> Maybe.map (.change >> roundToTwoDigits >> String.fromFloat)
                    |> Maybe.withDefault "0"
                )
            , text
                (event.gradeChange
                    |> Maybe.map (.partialScore >> roundToTwoDigits)
                    |> Maybe.withDefault finalGrade
                    |> String.fromFloat
                )
            , text
                (event.gradeChange
                    |> Maybe.map .reason
                    |> Maybe.withDefault ""
                )
            ]
    in
    table [ class "table" ]
        [ thead []
            [ headerRow ]
        , tbody []
            (events |> List.map eventRow)
        ]
