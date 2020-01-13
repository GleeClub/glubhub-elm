module Page.EditProfile exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, a, button, div, form, h4, input, label, option, p, section, select, span, text)
import Html.Attributes exposing (checked, class, disabled, for, name, placeholder, selected, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Json.Encode as Encode exposing (bool, int, null, object, string)
import MD5
import Maybe.Extra exposing (isJust, isNothing)
import Models.Event exposing (Member)
import Models.Info exposing (Enrollment(..), enrollmentToString)
import Route
import Task
import Time exposing (toYear)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), alert, postRequest)



---- MODEL ----


type alias Model =
    { common : Common
    , profileForm : ProfileForm
    , state : SubmissionState
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
            common.user
                |> Maybe.map formForUser
                |> Maybe.withDefault (defaultForm common)
    in
    ( { common = common
      , profileForm = profileForm
      , state = NotSentYet
      }
    , Cmd.none
    )


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


defaultForm : Common -> ProfileForm
defaultForm common =
    { firstName = ""
    , preferredName = ""
    , lastName = ""
    , email = ""
    , password = ""
    , confirmPassword = ""
    , phoneNumber = ""
    , location = ""
    , onCampus = True
    , major = ""
    , hometown = ""
    , passengers = 0
    , enrollment = Just Class
    , section = common.info.sections |> List.head
    , about = ""
    , picture = ""
    , arrivedAtTech = common.now |> toYear common.timeZone
    , gatewayDrug = ""
    , conflicts = ""
    , dietaryRestrictions = ""
    }



---- UPDATE ----


type Msg
    = EditForm ProfileForm
    | Submit
    | OnSubmit (GreaseResult ())
    | CancelEdit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        previousPage =
            case model.common.user of
                Just user ->
                    Route.Profile user.email

                Nothing ->
                    Route.Login
    in
    case msg of
        EditForm updatedForm ->
            ( { model | profileForm = updatedForm }, Cmd.none )

        Submit ->
            submit model

        OnSubmit (Ok _) ->
            ( model, Route.loadPage previousPage )

        OnSubmit (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )

        CancelEdit ->
            ( model, Route.loadPage previousPage )


submit : Model -> ( Model, Cmd Msg )
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
        ( model, alert "You need a section, bucko." )

    else
        case passHashResult of
            Err passwordError ->
                ( model, alert passwordError )

            Ok passHash ->
                let
                    profileJson =
                        serializeProfile model.profileForm passHash
                in
                if isJust model.common.user then
                    ( { model | state = Sending }
                    , updateMemberProfile profileJson model.common
                    )

                else
                    ( { model | state = Sending }
                    , registerNewMember profileJson model.common
                    )


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
        , ( "section", string <| Maybe.withDefault "" profile.section )
        , ( "enrollment"
          , profile.enrollment
                |> Maybe.map (enrollmentToString >> string)
                |> Maybe.withDefault null
          )
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
            [ Basics.title titleText
            , Basics.box
                [ headerText <| isJust model.common.user
                , viewForm model
                , case model.state of
                    ErrorSending error ->
                        Basics.errorBox error

                    _ ->
                        text ""
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
        profileForm =
            model.profileForm
    in
    form [ onSubmit Submit, style "margin-top" "1em" ]
        -- required fields
        [ h4 [ class "title is-4" ] [ text "Really Important Stuff" ]
        , horizontalField "Name"
            Nothing
            [ textField "firstName"
                profileForm.firstName
                "First"
                (\firstName -> EditForm { profileForm | firstName = firstName })
            , textField "preferredName"
                profileForm.preferredName
                "Preferred (optional)"
                (\preferredName -> EditForm { profileForm | preferredName = preferredName })
            , textField "lastName"
                profileForm.lastName
                "Last"
                (\lastName -> EditForm { profileForm | lastName = lastName })
            ]
        , horizontalField "E-mail"
            (Just "email")
            [ emailField "email"
                profileForm.email
                "gburdell3@gatech.edu"
                (\email -> EditForm { profileForm | email = email })
            ]
        , horizontalField "Phone Number"
            (Just "phone")
            [ phoneField "phone"
                profileForm.phoneNumber
                "6788675309"
                (\phoneNumber -> EditForm { profileForm | phoneNumber = phoneNumber })
            ]
        , horizontalField "Password"
            Nothing
            [ passwordField "password"
                profileForm.password
                "Password"
                (\password -> EditForm { profileForm | password = password })
            , passwordField "confirmPassword"
                profileForm.confirmPassword
                "Confirm Password"
                (\confirmPassword -> EditForm { profileForm | confirmPassword = confirmPassword })
            ]
        , horizontalField "Location" Nothing (locationFieldBlock profileForm)
        , horizontalField "Major"
            (Just "major")
            [ textField "major"
                profileForm.major
                "Undecided engineering"
                (\major -> EditForm { profileForm | major = major })
            ]
        , horizontalField "Hometown"
            (Just "hometown")
            [ textField "hometown"
                profileForm.hometown
                "Winslow, Arizona"
                (\hometown -> EditForm { profileForm | hometown = hometown })
            ]
        , horizontalField "Car"
            Nothing
            [ carFieldBlock profileForm ]
        , horizontalField "Enrollment"
            Nothing
            [ enrollmentBlock model.common profileForm ]

        -- optional fields
        , h4 [ class "title is-4" ] [ text "Nice to Know" ]
        , horizontalField "About Me"
            (Just "about")
            [ textField "about"
                profileForm.about
                "I like big butts and I cannot lie"
                (\about -> EditForm { profileForm | about = about })
            ]
        , horizontalField "Picture URL"
            (Just "picture")
            [ textField "picture"
                profileForm.picture
                "https://create.mylittlepony.movie/images/ponyparticon_bodybig.png"
                (\picture -> EditForm { profileForm | picture = picture })
            ]
        , horizontalField "Arrived at Tech"
            (Just "arrivedAtTech")
            [ numberField "arrivedAtTech"
                (String.fromInt profileForm.arrivedAtTech)
                "2099"
                (\arrivedAtTech ->
                    EditForm
                        { profileForm
                            | arrivedAtTech =
                                arrivedAtTech
                                    |> String.toInt
                                    |> Maybe.withDefault 0
                        }
                )
            ]
        , actionButtons (model.state == Sending) (model.common.user |> Maybe.map .email)
        ]


actionButtons : Bool -> Maybe String -> Html Msg
actionButtons sending maybeEmail =
    let
        backButton email =
            Basics.linkButton "Back" (Route.Profile email)

        saveButton =
            button
                [ type_ "submit"
                , class <|
                    "button is-primary"
                        ++ (if sending then
                                " is-loading"

                            else
                                ""
                           )
                ]
                [ text "Save" ]
    in
    div [ class "buttons is-right" ] <|
        (case maybeEmail of
            Just email ->
                [ backButton email ]

            Nothing ->
                []
        )
            ++ [ saveButton ]


carFieldBlock : ProfileForm -> Html Msg
carFieldBlock profileForm =
    div [ class "field is-grouped" ] <|
        [ p [ class "control checkbox" ]
            [ label [ class "checkbox" ]
                [ input
                    [ type_ "checkbox"
                    , name "hasCar"
                    , checked <| profileForm.passengers > 0
                    , onCheck
                        (\hasCar ->
                            EditForm
                                { profileForm
                                    | passengers =
                                        if hasCar then
                                            1

                                        else
                                            0
                                }
                        )
                    ]
                    []
                , text " I have a car"
                ]
            ]
        ]
            ++ passengersBlock profileForm


passengersBlock : ProfileForm -> List (Html Msg)
passengersBlock profileForm =
    if profileForm.passengers == 0 then
        []

    else
        [ div [ class "field has-addons" ]
            [ p [ class "control" ]
                [ input
                    [ class "input"
                    , type_ "number"
                    , name "passengers"
                    , value <| String.fromInt profileForm.passengers
                    , placeholder "How many?"
                    , onInput
                        (\passengers ->
                            EditForm
                                { profileForm
                                    | passengers =
                                        passengers
                                            |> String.toInt
                                            |> Maybe.withDefault 0
                                }
                        )
                    ]
                    []
                ]
            , p [ class "control" ]
                [ a [ class "button is-static" ] [ text "passengers" ] ]
            ]
        ]


locationFieldBlock : ProfileForm -> List (Html Msg)
locationFieldBlock profileForm =
    [ textField "location" profileForm.location "Glenn" (\location -> EditForm { profileForm | location = location })
    , p [ class "control" ]
        [ div [ class "buttons has-addons" ]
            [ span
                [ class <|
                    "button"
                        ++ (if profileForm.onCampus then
                                " is-primary"

                            else
                                ""
                           )
                , onClick <| EditForm { profileForm | onCampus = True }
                ]
                [ text "On-campus" ]
            , span
                [ class <|
                    "button"
                        ++ (if not profileForm.onCampus then
                                " is-primary"

                            else
                                ""
                           )
                , onClick <| EditForm { profileForm | onCampus = False }
                ]
                [ text "Off-campus" ]
            ]
        ]
    ]


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
                , onClick <| EditForm { profile | enrollment = enrollment }
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


enrollmentBlock : Common -> ProfileForm -> Html Msg
enrollmentBlock common profileForm =
    div [ class "field is-grouped" ]
        [ div [ class "control" ]
            [ div [ class "buttons has-addons" ]
                [ enrollmentOptions profileForm (isJust common.user) ]
            ]
        , div [ class "control" ]
            [ div [ class "select" ]
                [ select
                    [ onInput (\section -> EditForm { profileForm | section = Just section })
                    , value <| Maybe.withDefault "" profileForm.section
                    , disabled <| isNothing profileForm.enrollment
                    ]
                    (common.info.sections
                        |> List.map
                            (\sectionName ->
                                option
                                    [ value sectionName
                                    , selected
                                        (profileForm.section
                                            |> Maybe.map (\s -> s == sectionName)
                                            |> Maybe.withDefault False
                                        )
                                    ]
                                    [ text sectionName ]
                            )
                    )
                ]
            ]
        ]


horizontalField : String -> Maybe String -> (List (Html Msg) -> Html Msg)
horizontalField name forField =
    \content ->
        div [ class "field is-horizontal" ]
            [ div [ class "field-label is-normal" ]
                [ label
                    [ class "label"
                    , for (forField |> Maybe.withDefault "")
                    ]
                    [ text name ]
                ]
            , div [ class "field-body" ] content
            ]


fieldBlock : String -> String -> String -> String -> (String -> Msg) -> Html Msg
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


textField : String -> String -> String -> (String -> Msg) -> Html Msg
textField =
    fieldBlock "text"


numberField : String -> String -> String -> (String -> Msg) -> Html Msg
numberField =
    fieldBlock "number"


emailField : String -> String -> String -> (String -> Msg) -> Html Msg
emailField =
    fieldBlock "email"


phoneField : String -> String -> String -> (String -> Msg) -> Html Msg
phoneField =
    fieldBlock "phone"


passwordField : String -> String -> String -> (String -> Msg) -> Html Msg
passwordField =
    fieldBlock "password"



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
