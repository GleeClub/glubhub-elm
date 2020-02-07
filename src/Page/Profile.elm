module Page.Profile exposing (InternalMsg(..), Model, TranslationDictionary, Translator, init, translator, update, view)

import Browser.Navigation exposing (reload)
import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.DeleteModal exposing (deleteModal)
import Components.Forms as Forms exposing (radioInput, selectInput, textInput)
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, form, h1, i, img, li, p, section, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (find)
import Models.Event exposing (Member, memberDecoder)
import Models.Info exposing (Enrollment(..), enrollmentToString)
import Page.Profile.Attendance as Attendance
import Page.Profile.Money as Money
import Page.Profile.Semesters as Semesters
import Permissions
import Route exposing (ProfileRoute, ProfileTab(..))
import Task
import Utils
    exposing
        ( Common
        , RemoteData(..)
        , SubmissionState(..)
        , deleteRequest
        , fullName
        , getRequest
        , isActiveClass
        , mapLoaded
        , postRequest
        , setOldToken
        , setToken
        )



---- MODEL ----


type alias Model =
    { common : Common
    , member : RemoteData ( Member, FullProfileTab )
    , loginAsState : SubmissionState
    , deleteState : Maybe SubmissionState
    }


type FullProfileTab
    = FullProfileDetails (Maybe ProfileDetailsData)
    | FullProfileMoney Money.Model
    | FullProfileAttendance Attendance.Model
    | FullProfileSemesters Semesters.Model


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
    , arrivedAtTech : Maybe Int
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
    , arrivedAtTech = member.arrivedAtTech
    , gatewayDrug = member.gatewayDrug |> Maybe.withDefault ""
    , conflicts = member.conflicts |> Maybe.withDefault ""
    , dietaryRestrictions = member.dietaryRestrictions |> Maybe.withDefault ""
    }


init : Common -> ProfileRoute -> ( Model, Cmd Msg )
init common route =
    let
        model =
            { common = common
            , member = Loading
            , loginAsState = NotSentYet
            , deleteState = Nothing
            }
    in
    case common.members |> List.Extra.find (\member -> member.email == route.email) of
        Just member ->
            case route.tab of
                Just tab ->
                    changeTab
                        { model | member = Loaded ( member, FullProfileDetails Nothing ) }
                        tab

                Nothing ->
                    ( { model | member = Loaded ( member, FullProfileDetails Nothing ) }, Cmd.none )

        Nothing ->
            ( model, loadMember common route )



---- UPDATE ----


type Msg
    = ForSelf InternalMsg
    | ForParent OutMsg


type InternalMsg
    = OnLoadMember (GreaseResult ( Member, Maybe ProfileTab ))
    | Logout
    | LoginAsMember String
    | OnLoginAsMember (GreaseResult String)
    | ChangeDeleteState (Maybe SubmissionState)
    | DeleteMember String
    | OnDeleteMember (GreaseResult ())
    | ChangeTab ProfileTab
    | UpdateProfileForm ProfileForm
    | SendUpdateForm
    | OnUpdateMember (GreaseResult ( String, Member ))
    | AttendanceMsg Attendance.Msg
    | MoneyMsg Money.Msg
    | SemestersMsg Semesters.Msg


type OutMsg
    = UpdateCommon Common


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onUpdateCommon : Common -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMessage, onUpdateCommon } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal

        ForParent (UpdateCommon common) ->
            onUpdateCommon common


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadMember (Ok ( member, maybeTab )) ->
            case maybeTab of
                Just tab ->
                    changeTab { model | member = Loaded ( member, FullProfileDetails Nothing ) } tab

                Nothing ->
                    ( { model | member = Loaded ( member, FullProfileDetails Nothing ) }, Cmd.none )

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
            changeTab model newTab

        UpdateProfileForm newForm ->
            case model.member of
                Loaded ( member, _ ) ->
                    ( { model
                        | member =
                            Loaded
                                ( member
                                , FullProfileDetails <|
                                    Just { form = newForm, state = NotSentYet }
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SendUpdateForm ->
            case model.member of
                Loaded ( member, FullProfileDetails (Just data) ) ->
                    ( { model
                        | member =
                            Loaded
                                ( member
                                , FullProfileDetails <|
                                    Just { data | state = Sending }
                                )
                      }
                    , updateMember model.common member data.form
                    )

                _ ->
                    ( model, Cmd.none )

        OnUpdateMember (Ok ( oldEmail, updatedMember )) ->
            let
                common =
                    model.common

                memberMapper m =
                    if m.email == oldEmail then
                        updatedMember

                    else
                        m

                updatedCommon =
                    { common
                        | members =
                            common.members
                                |> List.map memberMapper
                                |> List.sortBy .lastName
                    }

                newModel =
                    { model
                        | common = updatedCommon
                        , member = Loaded ( updatedMember, FullProfileDetails Nothing )
                    }
            in
            changeTab newModel ProfileDetails
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Task.perform
                                (\_ -> ForParent <| UpdateCommon updatedCommon)
                                (Task.succeed (\_ -> ()))
                            ]
                    )

        OnUpdateMember (Err error) ->
            case model.member of
                Loaded ( member, FullProfileDetails (Just data) ) ->
                    ( { model
                        | member =
                            Loaded
                                ( member
                                , FullProfileDetails <|
                                    Just { data | state = ErrorSending error }
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AttendanceMsg tabMsg ->
            case model.member of
                Loaded ( member, FullProfileAttendance tabModel ) ->
                    let
                        ( newTabModel, newTabMsg ) =
                            Attendance.update tabMsg tabModel
                    in
                    ( { model | member = Loaded ( member, FullProfileAttendance newTabModel ) }
                    , newTabMsg |> Cmd.map (ForSelf << AttendanceMsg)
                    )

                _ ->
                    ( model, Cmd.none )

        MoneyMsg tabMsg ->
            case model.member of
                Loaded ( member, FullProfileMoney tabModel ) ->
                    let
                        ( newTabModel, newTabMsg ) =
                            Money.update tabMsg tabModel
                    in
                    ( { model | member = Loaded ( member, FullProfileMoney newTabModel ) }
                    , newTabMsg |> Cmd.map (ForSelf << MoneyMsg)
                    )

                _ ->
                    ( model, Cmd.none )

        SemestersMsg tabMsg ->
            case model.member of
                Loaded ( member, FullProfileSemesters tabModel ) ->
                    let
                        ( newTabModel, newTabMsg ) =
                            Semesters.update tabMsg tabModel
                    in
                    ( { model | member = Loaded ( member, FullProfileSemesters newTabModel ) }
                    , newTabMsg |> Cmd.map (ForSelf << SemestersMsg)
                    )

                _ ->
                    ( model, Cmd.none )


changeTab : Model -> ProfileTab -> ( Model, Cmd Msg )
changeTab model tab =
    case model.member of
        Loaded ( member, _ ) ->
            let
                ( tabModel, tabCmd ) =
                    case tab of
                        ProfileDetails ->
                            ( FullProfileDetails Nothing, Cmd.none )

                        ProfileMoney ->
                            Money.init model.common member
                                |> Tuple.mapFirst FullProfileMoney
                                |> Tuple.mapSecond (Cmd.map (ForSelf << MoneyMsg))

                        ProfileSemesters ->
                            Semesters.init model.common member
                                |> Tuple.mapFirst FullProfileSemesters
                                |> Tuple.mapSecond (Cmd.map (ForSelf << SemestersMsg))

                        ProfileAttendance ->
                            Attendance.init model.common member
                                |> Tuple.mapFirst FullProfileAttendance
                                |> Tuple.mapSecond (Cmd.map (ForSelf << AttendanceMsg))
            in
            ( { model
                | member = Loaded ( member, tabModel )
              }
            , Cmd.batch
                [ tabCmd
                , Route.replaceUrl model.common.key <|
                    Route.Profile { email = member.email, tab = Just tab }
                ]
            )

        _ ->
            ( model, Cmd.none )



---- DATA ----


loadMember : Common -> ProfileRoute -> Cmd Msg
loadMember common route =
    getRequest common ("/members/" ++ route.email) memberDecoder
        |> Task.map (\member -> ( member, route.tab ))
        |> Task.attempt (ForSelf << OnLoadMember)


loginAsMember : Common -> String -> Cmd Msg
loginAsMember common email =
    let
        url =
            "/members/" ++ email ++ "/login_as"

        decoder =
            Decode.field "token" Decode.string
    in
    getRequest common url decoder
        |> Task.attempt (ForSelf << OnLoginAsMember)


deleteMember : Common -> String -> Cmd Msg
deleteMember common email =
    let
        url =
            "/members/" ++ email ++ "?confirm=true"
    in
    deleteRequest common url
        |> Task.attempt (ForSelf << OnDeleteMember)


updateMember : Common -> Member -> ProfileForm -> Cmd Msg
updateMember common member memberForm =
    let
        body =
            serializeProfileForm member memberForm

        url =
            "/members/" ++ member.email

        newUrl =
            "/members/" ++ memberForm.email
    in
    postRequest common url body
        |> Task.andThen (\_ -> getRequest common newUrl memberDecoder)
        |> Task.map (\updatedMember -> ( member.email, updatedMember ))
        |> Task.attempt (ForSelf << OnUpdateMember)


serializeProfileForm : Member -> ProfileForm -> Encode.Value
serializeProfileForm member memberForm =
    Encode.object
        [ ( "email", Encode.string memberForm.email )
        , ( "firstName", Encode.string memberForm.firstName )
        , ( "preferredName", Encode.string memberForm.preferredName )
        , ( "lastName", Encode.string memberForm.lastName )
        , ( "phoneNumber", Encode.string memberForm.phoneNumber )
        , ( "picture", Encode.string memberForm.picture )
        , ( "passengers", Encode.int memberForm.passengers )
        , ( "location", Encode.string memberForm.location )
        , ( "onCampus", Encode.bool memberForm.onCampus )
        , ( "about", Encode.string memberForm.about )
        , ( "major", Encode.string memberForm.major )
        , ( "minor", Encode.string (member.minor |> Maybe.withDefault "") )
        , ( "hometown", Encode.string memberForm.hometown )
        , ( "arrivedAtTech"
          , memberForm.arrivedAtTech
                |> Maybe.map Encode.int
                |> Maybe.withDefault Encode.null
          )
        , ( "gatewayDrug", Encode.string memberForm.gatewayDrug )
        , ( "conflicts", Encode.string memberForm.conflicts )
        , ( "dietaryRestrictions", Encode.string memberForm.dietaryRestrictions )
        , ( "enrollment"
          , memberForm.enrollment
                |> Maybe.map (enrollmentToString >> Encode.string)
                |> Maybe.withDefault Encode.null
          )
        , ( "section", Encode.string (memberForm.section |> Maybe.withDefault "") )
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.section
            [ Basics.container
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
            Basics.section
                [ Basics.container
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
            , Buttons.group
                { alignment = Buttons.AlignLeft
                , connected = False
                , buttons = userActions model member
                }
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
        [ Buttons.button
            { content = "Log out"
            , onClick = Just <| ForSelf Logout
            , attrs = []
            }
        , Buttons.link
            { content = "Edit your profile"
            , route = Route.EditProfile
            , attrs = []
            }
        ]

    else
        [ Basics.renderIfHasPermission model.common Permissions.switchUser <|
            Buttons.button
                { content = "Log in as " ++ firstName
                , onClick = Just <| ForSelf <| LoginAsMember member.email
                , attrs = []
                }
        , Basics.renderIfHasPermission model.common Permissions.deleteUser <|
            Buttons.button
                { content = "Delete " ++ firstName
                , onClick = Just <| ForSelf <| ChangeDeleteState <| Just NotSentYet
                , attrs = []
                }
        ]


deleteMemberModal : Member -> SubmissionState -> Html Msg
deleteMemberModal member state =
    deleteModal
        { title = "Delete " ++ (member |> fullName) ++ "?"
        , cancel = ForSelf <| ChangeDeleteState Nothing
        , confirm = ForSelf <| DeleteMember member.email
        , state = state
        , content =
            div []
                [ text <| "Are you sure you want to delete " ++ (member |> fullName) ++ "?"
                , br [] []
                , i [] [ text "Think of what Uncle Ben would say. No, not the rice one." ]
                ]
        }


viewProfileTab : Model -> ( Member, FullProfileTab ) -> Html Msg
viewProfileTab model ( member, tab ) =
    div []
        [ profileTabList tab
        , case tab of
            FullProfileDetails currentForm ->
                profileDetailsTab model.common member currentForm

            FullProfileMoney tabModel ->
                Money.view tabModel |> Html.map (ForSelf << MoneyMsg)

            FullProfileSemesters tabModel ->
                Semesters.view tabModel |> Html.map (ForSelf << SemestersMsg)

            FullProfileAttendance tabModel ->
                Attendance.view tabModel |> Html.map (ForSelf << AttendanceMsg)
        ]


tabName : ProfileTab -> String
tabName tab =
    case tab of
        ProfileDetails ->
            "Details"

        ProfileMoney ->
            "Money"

        ProfileAttendance ->
            "Attendance"

        ProfileSemesters ->
            "Semesters"


profileTabList : FullProfileTab -> Html Msg
profileTabList currentTab =
    let
        allTabs =
            [ ProfileDetails, ProfileMoney, ProfileAttendance, ProfileSemesters ]
    in
    div [ class "tabs" ]
        [ ul []
            (allTabs
                |> List.map
                    (\tab ->
                        li [ class (isActiveClass (tabIsActive currentTab tab)) ]
                            [ a [ onClick (ForSelf <| ChangeTab tab) ]
                                [ text <| tabName tab ]
                            ]
                    )
            )
        ]


tabIsActive : FullProfileTab -> ProfileTab -> Bool
tabIsActive currentTab tab =
    case ( currentTab, tab ) of
        ( FullProfileDetails _, ProfileDetails ) ->
            True

        ( FullProfileMoney _, ProfileMoney ) ->
            True

        ( FullProfileAttendance _, ProfileAttendance ) ->
            True

        ( FullProfileSemesters _, ProfileSemesters ) ->
            True

        _ ->
            False


profileDetailsTab : Common -> Member -> Maybe ProfileDetailsData -> Html Msg
profileDetailsTab common member currentForm =
    case currentForm of
        Nothing ->
            readOnlyProfileDetails member

        Just memberForm ->
            editProfileForm common memberForm


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
    Basics.column
        [ table [ class "table" ]
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
        , Buttons.group
            { alignment = Buttons.AlignRight
            , connected = False
            , buttons =
                [ Buttons.button
                    { content = "Edit"
                    , onClick = Just <| ForSelf <| UpdateProfileForm <| formForMember member
                    , attrs = []
                    }
                ]
            }
        ]


editProfileForm : Common -> ProfileDetailsData -> Html Msg
editProfileForm common data =
    let
        memberForm =
            data.form

        onCampusToString onCampus =
            if onCampus then
                "Yes"

            else
                "No"
    in
    Basics.form (ForSelf SendUpdateForm)
        [ textInput Forms.string
            { value = memberForm.firstName
            , onInput = \firstName -> ForSelf <| UpdateProfileForm { memberForm | firstName = firstName }
            , attrs =
                [ Forms.Title "First Name"
                , Forms.RequiredField True
                , Forms.Horizontal
                ]
            }
        , textInput Forms.string
            { value = memberForm.preferredName
            , onInput = \preferredName -> ForSelf <| UpdateProfileForm { memberForm | preferredName = preferredName }
            , attrs =
                [ Forms.Title "Preferred Name"
                , Forms.Horizontal
                ]
            }
        , textInput Forms.string
            { value = memberForm.lastName
            , onInput = \lastName -> ForSelf <| UpdateProfileForm { memberForm | lastName = lastName }
            , attrs =
                [ Forms.Title "Last Name"
                , Forms.Horizontal
                ]
            }
        , textInput Forms.email
            { value = memberForm.email
            , onInput = \email -> ForSelf <| UpdateProfileForm { memberForm | email = email }
            , attrs =
                [ Forms.Title "Email"
                , Forms.Horizontal
                ]
            }
        , textInput Forms.string
            { value = memberForm.phoneNumber
            , onInput = \phoneNumber -> ForSelf <| UpdateProfileForm { memberForm | phoneNumber = phoneNumber }
            , attrs =
                [ Forms.Title "Phone Number"
                , Forms.Horizontal
                ]
            }
        , textInput Forms.string
            { value = memberForm.location
            , onInput = \location -> ForSelf <| UpdateProfileForm { memberForm | location = location }
            , attrs =
                [ Forms.Title "Location"
                , Forms.Horizontal
                ]
            }
        , radioInput onCampusToString
            { values = [ True, False ]
            , selected = memberForm.onCampus
            , onInput = \onCampus -> ForSelf <| UpdateProfileForm { memberForm | onCampus = onCampus }
            , attrs =
                [ Forms.Title "On Campus?"
                , Forms.Horizontal
                ]
            }
        , selectInput Forms.enrollment
            { values = [ Just Class, Just Club, Nothing ]
            , selected = memberForm.enrollment
            , onInput = \enrollment -> ForSelf <| UpdateProfileForm { memberForm | enrollment = enrollment }
            , attrs =
                [ Forms.Title "Enrollment"
                , Forms.Horizontal
                ]
            }
        , selectInput (Forms.section common)
            { values = Nothing :: (common.info.sections |> List.map Just)
            , selected = memberForm.section
            , onInput = \section -> ForSelf <| UpdateProfileForm { memberForm | section = section }
            , attrs =
                [ Forms.Title "Section"
                , Forms.Horizontal
                ]
            }
        , textInput Forms.string
            { value = memberForm.about
            , onInput = \about -> ForSelf <| UpdateProfileForm { memberForm | about = about }
            , attrs =
                [ Forms.Title "About"
                , Forms.Horizontal
                ]
            }
        , textInput Forms.string
            { value = memberForm.picture
            , onInput = \picture -> ForSelf <| UpdateProfileForm { memberForm | picture = picture }
            , attrs =
                [ Forms.Title "Picture"
                , Forms.Horizontal
                ]
            }
        , textInput Forms.int
            { value = memberForm.arrivedAtTech
            , onInput = \arrivedAtTech -> ForSelf <| UpdateProfileForm { memberForm | arrivedAtTech = arrivedAtTech }
            , attrs =
                [ Forms.Title "Arrived At Tech"
                , Forms.Horizontal
                ]
            }
        , textInput Forms.string
            { value = memberForm.gatewayDrug
            , onInput = \gatewayDrug -> ForSelf <| UpdateProfileForm { memberForm | gatewayDrug = gatewayDrug }
            , attrs =
                [ Forms.Title "Gateway Drug"
                , Forms.Horizontal
                ]
            }
        , textInput Forms.string
            { value = memberForm.conflicts
            , onInput = \conflicts -> ForSelf <| UpdateProfileForm { memberForm | conflicts = conflicts }
            , attrs =
                [ Forms.Title "Conflicts"
                , Forms.Horizontal
                ]
            }
        , textInput Forms.string
            { value = memberForm.dietaryRestrictions
            , onInput = \dietaryRestrictions -> ForSelf <| UpdateProfileForm { memberForm | dietaryRestrictions = dietaryRestrictions }
            , attrs =
                [ Forms.Title "Dietary Restrictions"
                , Forms.Horizontal
                ]
            }
        , Buttons.group
            { alignment = Buttons.AlignRight
            , connected = False
            , buttons =
                [ Buttons.button
                    { content = "Cancel"
                    , onClick = Just <| ForSelf <| ChangeTab ProfileDetails
                    , attrs = []
                    }
                , Buttons.submit
                    { content = "Save"
                    , attrs =
                        [ Buttons.Color Buttons.IsPrimary
                        , Buttons.IsLoading (data.state == Sending)
                        ]
                    }
                ]
            }
        ]
