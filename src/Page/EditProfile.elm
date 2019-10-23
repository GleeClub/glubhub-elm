module Page.EditProfile exposing (Model, Msg(..), ProfileForm, actionButtons, defaultForm, enrollmentOptions, formForUser, headerText, horizontalField, init, update, view, viewForm)

import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, img, input, label, section, span, text)
import Html.Attributes exposing (class, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (int, string, object)
import MD5
import Models.Member exposing (Member, memberDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), alert, apiUrl, notFoundView, setToken, spinner)
import Maybe.Extra exposing (isJust, isNothing)


---- MODEL ----


type alias Model =
    { common : Maybe Common
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
    , arrivedAtTech : String
    , gatewayDrug : String
    , conflicts : String
    , dietaryRestrictions : String
    }


init : Maybe Common -> ( Model, Cmd Msg )
init common =
    let
        profileForm =
            case common |> Maybe.map (\c -> c.user) of
                Just user ->
                    formForUser user

                Nothing ->
                    defaultForm
    in
    ( { common = common, profileForm = profileForm }, Cmd.none )


formForUser : Member -> ProfileForm
formForUser user =
    { user | password = "", confirmPassword = "" }


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
    , onCampus = false
    , major = ""
    , hometown = ""
    , passengers = 0
    , enrollment = Just Class
    , section = Nothing
    , about = ""
    , picture = ""
    , arrivedAtTech = ""
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
    | EditEnrollment Maybe Enrollment
    | EditSection Maybe String
    | EditAbout String
    | EditPicture String
    | EditArrivedAtTech Int
    | EditGatewayDrug String
    | EditConflicts String
    | EditDietaryRestrictions String
    | Submit
    | BackToLogin


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        profileForm =
            model.profileForm
    in
    case msg of
        EditFirstName firstName ->
            ( { model | profileForm = { profileForm | firstName = firstName } }, Cmd.None )

        EditPreferredName preferredName ->
            ( { model | profileForm = { profileForm | preferredName = preferredName } }, Cmd.None )

        EditLastName lastName ->
            ( { model | profileForm = { profileForm | lastName = lastName } }, Cmd.None )

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
            ( { model | profileForm = { profileForm | section = section } }, Cmd.none )

        EditAbout about ->
            ( { model | profileForm = { profileForm | about = about } }, Cmd.none )

        EditPicture picture ->
            ( { model | profileForm = { profileForm | picture = picture } }, Cmd.none )

        EditArrivedAtTech arrivedAtTech ->
            ( { model | profileForm = { profileForm | arrivedAtTech = arrivedAtTech } }, Cmd.none )

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
            String.length profileForm.password > 0 or String.length profileForm.confirmPassword > 0

        passHash =
            if not enteredPassword then
                if isJust model.common then
                    Ok Nothing
                else
                    Err "You must enter a password."

            else if profileForm.password == profileForm.confirmPassword then
                Ok Just <| MD5.hex profileForm.password

            else
                Err "Your passwords don't match."
    in
    if isNothing model.profileForm.section then
        alert "You need a section, bucko."
    else 
      case ( passHash, model.common ) of
          ( Ok passHash, Just common ) ->
              updateMemberProfile model.profileForm passHash common

          ( Ok passHash, Nothing ) ->
              registerNewMember model.profileForm passHash

          ( Err passwordError, _ ) ->
              alert passwordError


updateMemberProfile : ProfileForm -> Maybe String -> Common -> Cmd Msg
updateMemberProfile profile passHash common =
    let
        requestBody =
            object
                [ ("firstName", string profile.firstName)
                , ("preferredName", string profile.preferredName)
                , ("lastName", string profile.lastName)
                , ("passHash", string <| Maybe.withDefault "" passHash)
                , ("phoneNumber", string profile.phoneNumber)
                , ("picture", string picture)
                , ("passengers", int profile.passengers)
                , ("location", string profile.location)
                , ("onCampus", string <| if profile.onCampus then "true" else "false")
                , ("about", string profile.about)
                , ("major", string profile.major)
                , ("hometown", string profile.hometown)
                , ("arrivedAtTech", int profile.arrivedAtTech)
                , ("gatewayDrug", )
                ]
    pub passengers: i32,
    pub location: String,
    #[serde(default, rename = "onCampus")]
    pub on_campus: Option<bool>,
    #[serde(default, deserialize_with = "deser_opt_string")]
    pub about: Option<String>,
    #[serde(default, deserialize_with = "deser_opt_string")]
    pub major: Option<String>,
    #[serde(default, deserialize_with = "deser_opt_string")]
    pub minor: Option<String>,
    #[serde(default, deserialize_with = "deser_opt_string")]
    pub hometown: Option<String>,
    #[serde(default, rename = "arrivedAtTech")]
    pub arrived_at_tech: Option<i32>,
    #[serde(default, rename = "gatewayDrug", deserialize_with = "deser_opt_string")]
    pub gateway_drug: Option<String>,
    #[serde(default, deserialize_with = "deser_opt_string")]
    pub conflicts: Option<String>,
    #[serde(default, rename = "dietaryRestrictions", deserialize_with = "deser_opt_string")]
    pub dietary_restrictions: Option<String>,
    #[serde(default)]
    pub enrollment: Option<Enrollment>,
    #[serde(default, deserialize_with = "deser_opt_string")]
    pub section: Option<String>,
    in
    

registerNewMember : ProfileForm -> Maybe String -> Cmd Msg



-- public submit(): void {
--   this.profile.passengers = this.hasCar ? this.profile.passengers : 0;
--   if (this.password || this.password2) {
--     if (this.password === this.password2) {
--       this.profile.passHash = md5(this.password);
--     } else {
--       alert('Your passwords don\'t match.');
--       return;
--     }
--   } else {
--     this.profile.passHash = null;
--   }
--   if (!this.profile.section) {
--     alert('You need to pick a section.');
--     return;
--   }
--   this.common.apiPost('members/profile', this.profile, (data) => {
--     Router.push('/');
--     this.$emit('reload');
--   }, (error) => {
--     alert(error.message);
--   });
-- }
---- VIEW ----


view : Model -> Html Msg
view model =
    let
        isLoggedIn =
            isJust model.common

        titleText =
            (if isJust model.common then
                "Edit"

             else
                "Create"
            )
                ++ "Profile"
    in
    section [ class "section" ]
        [ div [ class "container" ]
            [ h3 [ class "title is-3" ] [ text titleText ]
            , div [ id "edit-profile", class "box" ]
                [ headerText isLoggedIn
                , viewForm model.profileForm
                ]
            ]
        ]


headerText : Bool -> Html Msg
headerText isLoggedIn =
    if isLoggedIn then
        p []
            [ text "You can make changes to your stats here."
            , text "It's important we know as much about you as possible to"
            , span [ style "text-decoration" "line-through" ] [ text "creep" ]
            , span [ style "text-decoration" "line-through" ] [ text "better serve you" ]
            , text "make you drive carpools. It'll also help your new friends get to know you!"
            ]

    else
        p []
            [ text "Note that this registration is not mandatory. If you are unwilling to"
            , text "provide any of the required information, let an officer know and"
            , text "we will work out alternate means of registration."
            ]


viewForm : ProfileForm -> Html Msg
viewForm profile =
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
            [ textField "location" profile.location "Glenn" EditLocation
            , p [ class "control" ]
                [ div class "buttons has-addons" ]
                [ span
                    [ class "button"
                        ++ (if profile.onCampus then
                                " is-primary"

                            else
                                ""
                           )
                    , onClick <| \_ -> EditOnCampus True
                    ]
                    [ text "On-campus" ]
                , span
                    [ class "button"
                        ++ (if not profile.onCampus then
                                " is-primary"

                            else
                                ""
                           )
                    , onClick <| \_ -> EditOnCampus False
                    ]
                    [ text "Off-campus" ]
                ]
            ]

        passengersBlock =
            if profile.passengers == 0 then
                []

            else
                [ div [ class "field has-addons" ]
                    [ p [ class "control" ]
                        [ input
                            [ class "input"
                            , type_ "number"
                            , name "passengers"
                            , value <| String.fromInt profile.passengers
                            , placeholder "How many?"
                            ]
                            []
                        ]
                    , p [ class "control" ]
                        [ a [ class "button is-static" ] [ text "passengers" ] ]
                    ]
                ]

        carFieldBlock =
            div [ class "field is-grouped" ]
                [ p [ class "control checkbox" ]
                    [ label [ class "checkbox" ]
                        [ input [ type_ "checkbox", name "hasCar", isChecked profile.passengers > 0 ]
                        , text "I have a car"
                        ]
                    ]
                ]
                ++ passengersBlock

        enrollmentBlock =
            div [ class "field is-grouped" ]
                [ div [ class "control" ]
                    [ div [ class "buttons has-addons" ]
                        [ enrollmentOption "Inactive" Nothing
                        , enrollmentOption "Class" Just Class
                        , enrollmentOption "Club" Just Club
                        ]
                    ]
                , div [ class "control" ]
                    [ div [ class "select" ]
                        [ select
                            [ onSelect EditSection
                            , value <| Maybe.withDefault "" profile.section
                            , disabled <| isNothing profile.enrollment
                            ]
                            common.info.sections
                            |> List.map (\sectionName -> option [ value sectionName ] [ text sectionName ])
                        ]
                    ]
                ]
    in
    form [ onSubmit Submit, style "margin-top" "1em" ]
        -- required fields
        [ h4 [ class "title is-4" ] [ text "Really Important Stuff" ]
        , horizontalField "Name"
            Nothing
            [ textField "firstName" profile.firstName "First" EditFirstName
            , textField "preferredName" profile.preferredName "Preferred (optional)" EditPreferredName
            , textField "lastName" profile.lastName "Last" EditLastName
            ]
        , horizontalField "E-mail"
            Just
            "email"
            [ emailField "email" profile.email "gburdell3@gatech.edu" EditEmail ]
        , horizontalField "Phone Number"
            Just
            "phone"
            [ phoneField "phone" profile.phoneNumber "6788675309" EditPhoneNumber ]
        , horizontalField "Password"
            Nothing
            [ passwordField "password" profile.password "Password" EditPassword
            , passwordField "confirmPassword" profile.confirmPassword "Confirm Password" EditConfirmPassword
            ]
        , horizontalField "Location" Nothing locationFieldBlock
        , horizontalField "Major"
            Just
            "major"
            [ textField "major" profile.major "Undecided engineering" EditMajor ]
        , horizontalField "Hometown"
            Just
            "hometown"
            [ textField "hometown" profile.hometown "Winslow, Arizona" EditHometown ]
        , horizontalField "Car"
            Nothing
            [ carFieldBlock ]
        , horizontalField "Enrollment"
            Nothing
            [ enrollmentBlock ]

        -- optional fields
        , h4 [ class "title is-4" ] [ text "Nice to Know" ]
        , horizontalField "About Me"
            Just
            "about"
            [ textField "about" profile.about "I like big butts and I cannot lie" EditAbout ]
        , horizontalField "Picture URL"
            Just
            "picture"
            [ textField "picture" profile.picture "https://create.mylittlepony.movie/images/ponyparticon_bodybig.png" EditPicture ]
        , horizontalField "Arrived at Tech"
            Just
            "arrivedAtTech"
            [ numberField "arrivedAtTech" String.fromInt profile.arrivedAtTech "2099" EditArrivedAtTech ]
        , actionButtons
        ]


horizontalField : String -> Maybe String -> (List (Html Msg) -> Html Msg)
horizontalField name forField =
    \content ->
        div [ class "field is-horizontal" ]
            [ div [ class "field-label is-normal" ]
                [ label [ class "label", for (Maybe.withDefault "" forField) ] [ text name ] ]
            , div [ class "field-body" ] content
            ]


actionButtons : Bool -> Html Msg
actionButtons loggedIn =
    let
        backButton =
            button [ type_ "button", class "button", onClick BackToLogin ] [ text "Back" ]

        saveButton =
            button [ type_ "submit", class "button is-primary", onClick Save ] [ text "Save" ]
    in
    div [ class "buttons is-right" ] <|
        (if loggedIn then
            [ backButton ]

         else
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
                [ class <| buttonClass profile.enrollment == enrollment
                , onClick EditEnrollment enrollment
                ]
                [ text optionName ]
    in
    div [ class "control" ]
        [ div [ class "buttons has-addons" ] List.concat <|
            [ if isLoggedIn then
                [ enrollmentOption "Inactive" Nothing ]

              else
                []
            , [ enrollmentOption "Class" Just Class ]
            , [ enrollmentOption "Club" Just Club ]
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
