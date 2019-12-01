module Page.EditProfile exposing (Model, Msg(..), ProfileForm, actionButtons, defaultForm, enrollmentOptions, formForUser, headerText, horizontalField, init, update, view, viewForm)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, a, button, div, form, h1, h3, h4, img, input, label, option, p, section, select, span, text)
import Html.Attributes exposing (checked, class, disabled, for, href, id, name, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (bool, int, object, string)
import MD5
import Maybe.Extra exposing (isJust, isNothing)
import Models.Event exposing (Member, memberDecoder)
import Models.Info exposing (Enrollment(..), enrollmentToString)
import Route exposing (Route)
import Task
import Utils exposing (Common, RemoteData(..), alert, apiUrl, postRequest, setToken)



---- MODEL ----


type alias Model =
    { common : Common
    , profileForm : ProfileForm
    }


type alias ProfileForm =
    { firstName : String
    , preferredName : String
    , lastName : String
    , email : String
    , password : String
    , confirmPassword : String
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


init : Common -> ( Model, Cmd Msg )
init common =
    let
        profileForm =
            case common.user of
                Just user ->
                    formForUser user

                Nothing ->
                    defaultForm
    in
    ( { common = common, profileForm = profileForm }, Cmd.none )


formForUser : Member -> ProfileForm
formForUser user =
    { firstName = user.firstName
    , preferredName = user.preferredName |> Maybe.withDefault ""
    , lastName = user.lastName
    , email = user.email
    , password = ""
    , confirmPassword = ""
    , phoneNumber = user.phoneNumber
    , location = user.location
    , onCampus = user.onCampus |> Maybe.withDefault False
    , major = user.major |> Maybe.withDefault ""
    , hometown = user.hometown |> Maybe.withDefault ""
    , passengers = user.passengers
    , enrollment = user.enrollment
    , section = user.section
    , about = user.about |> Maybe.withDefault ""
    , picture = user.picture |> Maybe.withDefault ""
    , arrivedAtTech = user.arrivedAtTech |> Maybe.withDefault 1
    , gatewayDrug = user.gatewayDrug |> Maybe.withDefault ""
    , conflicts = user.conflicts |> Maybe.withDefault ""
    , dietaryRestrictions = user.dietaryRestrictions |> Maybe.withDefault ""
    }


defaultForm : ProfileForm
defaultForm =
    { firstName = ""
    , preferredName = ""
    , lastName = ""
    , email = ""
    , password = ""
    , confirmPassword = ""
    , phoneNumber = ""
    , location = ""
    , onCampus = False
    , major = ""
    , hometown = ""
    , passengers = 0
    , enrollment = Just Class
    , section = Nothing
    , about = ""
    , picture = ""
    , arrivedAtTech = 1
    , gatewayDrug = ""
    , conflicts = ""
    , dietaryRestrictions = ""
    }



---- UPDATE ----


type Msg
    = EditFirstName String
    | EditPreferredName String
    | EditLastName String
    | EditEmail String
    | EditPassword String
    | EditConfirmPassword String
    | EditPhoneNumber String
    | EditLocation String
    | EditOnCampus Bool
    | EditMajor String
    | EditHometown String
    | EditPassengers Int
    | EditEnrollment (Maybe Enrollment)
    | EditSection String
    | EditAbout String
    | EditPicture String
    | EditArrivedAtTech String
    | EditGatewayDrug String
    | EditConflicts String
    | EditDietaryRestrictions String
    | Submit
    | OnSubmit (GreaseResult ())
    | BackToLogin


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        profileForm =
            model.profileForm
    in
    case msg of
        EditFirstName firstName ->
            ( { model | profileForm = { profileForm | firstName = firstName } }, Cmd.none )

        EditPreferredName preferredName ->
            ( { model | profileForm = { profileForm | preferredName = preferredName } }, Cmd.none )

        EditLastName lastName ->
            ( { model | profileForm = { profileForm | lastName = lastName } }, Cmd.none )

        EditEmail email ->
            ( { model | profileForm = { profileForm | email = email } }, Cmd.none )

        EditPassword password ->
            ( { model | profileForm = { profileForm | password = password } }, Cmd.none )

        EditConfirmPassword confirmPassword ->
            ( { model | profileForm = { profileForm | confirmPassword = confirmPassword } }, Cmd.none )

        EditPhoneNumber phoneNumber ->
            ( { model | profileForm = { profileForm | phoneNumber = phoneNumber } }, Cmd.none )

        EditLocation location ->
            ( { model | profileForm = { profileForm | location = location } }, Cmd.none )

        EditOnCampus onCampus ->
            ( { model | profileForm = { profileForm | onCampus = onCampus } }, Cmd.none )

        EditMajor major ->
            ( { model | profileForm = { profileForm | major = major } }, Cmd.none )

        EditHometown hometown ->
            ( { model | profileForm = { profileForm | hometown = hometown } }, Cmd.none )

        EditPassengers passengers ->
            ( { model | profileForm = { profileForm | passengers = passengers } }, Cmd.none )

        EditEnrollment enrollment ->
            ( { model | profileForm = { profileForm | enrollment = enrollment } }, Cmd.none )

        EditSection section ->
            ( { model | profileForm = { profileForm | section = Just section } }, Cmd.none )

        EditAbout about ->
            ( { model | profileForm = { profileForm | about = about } }, Cmd.none )

        EditPicture picture ->
            ( { model | profileForm = { profileForm | picture = picture } }, Cmd.none )

        EditArrivedAtTech arrivedAtTech ->
            ( { model | profileForm = { profileForm | arrivedAtTech = String.toInt arrivedAtTech |> Maybe.withDefault 1 } }, Cmd.none )

        EditGatewayDrug gatewayDrug ->
            ( { model | profileForm = { profileForm | gatewayDrug = gatewayDrug } }, Cmd.none )

        EditConflicts conflicts ->
            ( { model | profileForm = { profileForm | conflicts = conflicts } }, Cmd.none )

        EditDietaryRestrictions dietaryRestrictions ->
            ( { model | profileForm = { profileForm | dietaryRestrictions = dietaryRestrictions } }, Cmd.none )

        Submit ->
            ( model, submit model )

        OnSubmit (Ok _) ->
            ( model, Route.loadPage Route.Login )

        OnSubmit (Err error) ->
            ( model, alert "Whoops..." )

        BackToLogin ->
            ( model, Route.loadPage Route.Login )


submit : Model -> Cmd Msg
submit model =
    let
        enteredPassword =
            (String.length model.profileForm.password > 0) || (String.length model.profileForm.confirmPassword > 0)

        passHashResult =
            if not enteredPassword then
                if isJust model.common.user then
                    Ok Nothing

                else
                    Err "You must enter a password."

            else if model.profileForm.password == model.profileForm.confirmPassword then
                Ok <| Just <| MD5.hex model.profileForm.password

            else
                Err "Your passwords don't match."
    in
    if isNothing model.profileForm.section then
        alert "You need a section, bucko."

    else
        case passHashResult of
            Err passwordError ->
                alert passwordError

            Ok passHash ->
                let
                    profileJson =
                        serializeProfile model.profileForm passHash
                in
                if isJust model.common.user then
                    updateMemberProfile profileJson model.common

                else
                    registerNewMember profileJson model.common


updateMemberProfile : Encode.Value -> Common -> Cmd Msg
updateMemberProfile profile common =
    postRequest common "/members/profile" profile
        |> Task.attempt OnSubmit


registerNewMember : Encode.Value -> Common -> Cmd Msg
registerNewMember profile common =
    postRequest common "/members" profile
        |> Task.attempt OnSubmit


serializeProfile : ProfileForm -> Maybe String -> Encode.Value
serializeProfile profile passHash =
    object
        [ ( "email", string profile.email )
        , ( "firstName", string profile.firstName )
        , ( "preferredName", string profile.preferredName )
        , ( "lastName", string profile.lastName )
        , ( "passHash", string <| Maybe.withDefault "" passHash )
        , ( "phoneNumber", string profile.phoneNumber )
        , ( "picture", string profile.picture )
        , ( "passengers", int profile.passengers )
        , ( "location", string profile.location )
        , ( "onCampus", bool profile.onCampus )
        , ( "about", string profile.about )
        , ( "major", string profile.major )
        , ( "hometown", string profile.hometown )
        , ( "arrivedAtTech", int profile.arrivedAtTech )
        , ( "gatewayDrug", string profile.gatewayDrug )
        , ( "conflicts", string profile.conflicts )
        , ( "dietaryRestrictions", string profile.dietaryRestrictions )
        , ( "enrollment", string <| (profile.enrollment |> Maybe.map enrollmentToString |> Maybe.withDefault "") )
        , ( "section", string <| Maybe.withDefault "" profile.section )
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        titleText =
            (if isJust model.common.user then
                "Edit"

             else
                "Create"
            )
                ++ " Profile"
    in
    section [ class "section" ]
        [ div [ class "container" ]
            [ h3 [ class "title is-3" ] [ text titleText ]
            , div [ id "edit-profile", class "box" ]
                [ headerText <| isJust model.common.user
                , viewForm model
                ]
            ]
        ]


headerText : Bool -> Html Msg
headerText isLoggedIn =
    if isLoggedIn then
        p []
            [ text "You can make changes to your stats here. "
            , text "It's important we know as much about you as possible to "
            , span [ style "text-decoration" "line-through" ] [ text "creep" ]
            , text " "
            , span [ style "text-decoration" "line-through" ] [ text "better serve you" ]
            , text " make you drive carpools. It'll also help your new friends get to know you!"
            ]

    else
        p []
            [ text "Note that this registration is not mandatory. If you are unwilling to "
            , text "provide any of the required information, let an officer know and "
            , text "we will work out alternate means of registration."
            ]


viewForm : Model -> Html Msg
viewForm model =
    let
        fieldBlock fieldType fieldName val placeholderText msg =
            div [ class "field" ]
                [ p [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ fieldType
                        , name fieldName
                        , value val
                        , placeholder placeholderText
                        , onInput msg
                        ]
                        []
                    ]
                ]

        textField =
            fieldBlock "text"

        numberField =
            fieldBlock "number"

        emailField =
            fieldBlock "email"

        phoneField =
            fieldBlock "phone"

        passwordField =
            fieldBlock "password"

        locationFieldBlock =
            [ textField "location" model.profileForm.location "Glenn" EditLocation
            , p [ class "control" ]
                [ div [ class "buttons has-addons" ]
                    [ span
                        [ class <|
                            "button"
                                ++ (if model.profileForm.onCampus then
                                        " is-primary"

                                    else
                                        ""
                                   )
                        , onClick <| EditOnCampus True
                        ]
                        [ text "On-campus" ]
                    , span
                        [ class <|
                            "button"
                                ++ (if not model.profileForm.onCampus then
                                        " is-primary"

                                    else
                                        ""
                                   )
                        , onClick <| EditOnCampus False
                        ]
                        [ text "Off-campus" ]
                    ]
                ]
            ]

        passengersBlock =
            if model.profileForm.passengers == 0 then
                []

            else
                [ div [ class "field has-addons" ]
                    [ p [ class "control" ]
                        [ input
                            [ class "input"
                            , type_ "number"
                            , name "passengers"
                            , value <| String.fromInt model.profileForm.passengers
                            , placeholder "How many?"
                            ]
                            []
                        ]
                    , p [ class "control" ]
                        [ a [ class "button is-static" ] [ text "passengers" ] ]
                    ]
                ]

        carFieldBlock =
            div [ class "field is-grouped" ] <|
                [ p [ class "control checkbox" ]
                    [ label [ class "checkbox" ]
                        [ input [ type_ "checkbox", name "hasCar", checked <| model.profileForm.passengers > 0 ] []
                        , text " I have a car"
                        ]
                    ]
                ]
                    ++ passengersBlock

        enrollmentBlock =
            div [ class "field is-grouped" ]
                [ div [ class "control" ]
                    [ div [ class "buttons has-addons" ]
                        [ enrollmentOptions model.profileForm (isJust model.common.user) ]
                    ]
                , div [ class "control" ]
                    [ div [ class "select" ]
                        [ select
                            [ onInput EditSection
                            , value <| Maybe.withDefault "" model.profileForm.section
                            , disabled <| isNothing model.profileForm.enrollment
                            ]
                            (model.common.info.sections
                                |> List.map (\sectionName -> option [ value sectionName ] [ text sectionName ])
                            )
                        ]
                    ]
                ]
    in
    form [ onSubmit Submit, style "margin-top" "1em" ]
        -- required fields
        [ h4 [ class "title is-4" ] [ text "Really Important Stuff" ]
        , horizontalField "Name"
            Nothing
            [ textField "firstName" model.profileForm.firstName "First" EditFirstName
            , textField "preferredName" model.profileForm.preferredName "Preferred (optional)" EditPreferredName
            , textField "lastName" model.profileForm.lastName "Last" EditLastName
            ]
        , horizontalField "E-mail"
            (Just "email")
            [ emailField "email" model.profileForm.email "gburdell3@gatech.edu" EditEmail ]
        , horizontalField "Phone Number"
            (Just "phone")
            [ phoneField "phone" model.profileForm.phoneNumber "6788675309" EditPhoneNumber ]
        , horizontalField "Password"
            Nothing
            [ passwordField "password" model.profileForm.password "Password" EditPassword
            , passwordField "confirmPassword" model.profileForm.confirmPassword "Confirm Password" EditConfirmPassword
            ]
        , horizontalField "Location" Nothing locationFieldBlock
        , horizontalField "Major"
            (Just "major")
            [ textField "major" model.profileForm.major "Undecided engineering" EditMajor ]
        , horizontalField "Hometown"
            (Just "hometown")
            [ textField "hometown" model.profileForm.hometown "Winslow, Arizona" EditHometown ]
        , horizontalField "Car"
            Nothing
            [ carFieldBlock ]
        , horizontalField "Enrollment"
            Nothing
            [ enrollmentBlock ]

        -- optional fields
        , h4 [ class "title is-4" ] [ text "Nice to Know" ]
        , horizontalField "About Me"
            (Just "about")
            [ textField "about" model.profileForm.about "I like big butts and I cannot lie" EditAbout ]
        , horizontalField "Picture URL"
            (Just "picture")
            [ textField "picture" model.profileForm.picture "https://create.mylittlepony.movie/images/ponyparticon_bodybig.png" EditPicture ]
        , horizontalField "Arrived at Tech"
            (Just "arrivedAtTech")
            [ numberField "arrivedAtTech" (String.fromInt model.profileForm.arrivedAtTech) "2099" EditArrivedAtTech ]
        , actionButtons <| (model.common.user |> Maybe.map .email)
        ]


horizontalField : String -> Maybe String -> (List (Html Msg) -> Html Msg)
horizontalField name forField =
    \content ->
        div [ class "field is-horizontal" ]
            [ div [ class "field-label is-normal" ]
                [ label [ class "label", for (Maybe.withDefault "" forField) ] [ text name ] ]
            , div [ class "field-body" ] content
            ]


actionButtons : Maybe String -> Html Msg
actionButtons maybeEmail =
    let
        backButton email =
            Basics.linkButton "Back" (Route.Profile email)

        saveButton =
            button [ type_ "submit", class "button is-primary" ] [ text "Save" ]
    in
    div [ class "buttons is-right" ] <|
        (case maybeEmail of
            Just email ->
                [ backButton email ]

            Nothing ->
                []
        )
            ++ [ saveButton ]


enrollmentOptions : ProfileForm -> Bool -> Html Msg
enrollmentOptions profile isLoggedIn =
    let
        buttonClass isPrimary =
            "button"
                ++ (if isPrimary then
                        " is-primary"

                    else
                        ""
                   )

        enrollmentOption optionName enrollment =
            span
                [ class <| buttonClass <| profile.enrollment == enrollment
                , onClick <| EditEnrollment enrollment
                ]
                [ text optionName ]
    in
    div [ class "control" ]
        [ div [ class "buttons has-addons" ] <|
            List.concat
                [ if isLoggedIn then
                    [ enrollmentOption "Inactive" Nothing ]

                  else
                    []
                , [ enrollmentOption "Class" (Just Class) ]
                , [ enrollmentOption "Club" (Just Club) ]
                ]
        ]



-- <style>
-- .buttons
-- {
--   margin-bottom: 0 !important;
-- }
-- .buttons .button
-- {
--   margin-bottom: 0;
-- }
-- input[type$="number"]
-- {
--   max-width: 10em;
-- }
-- p.control.checkbox
-- {
--   height: 2.25em;
--   padding-top: 0.5em;
--   padding-left: 1em;
--   padding-right: 1em;
-- }
-- </style>
