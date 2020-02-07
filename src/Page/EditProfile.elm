module Page.EditProfile exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.Forms as Forms exposing (checkboxInput, inputWrapper, selectInput, textInput)
import Error exposing (GreaseResult)
import Html exposing (Html, br, div, p, span, text)
import Html.Attributes exposing (class, style)
import Json.Encode as Encode exposing (bool, int, null, object, string)
import MD5
import Maybe.Extra as Maybe exposing (isJust, isNothing)
import Models.Event exposing (Member)
import Models.Info exposing (Enrollment(..), enrollmentToString)
import Request
import Route
import Task
import Time exposing (toYear)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), alert)



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
    , arrivedAtTech = currentYear common
    , gatewayDrug = ""
    , conflicts = ""
    , dietaryRestrictions = ""
    }


currentYear : Common -> Int
currentYear common =
    common.now |> toYear common.timeZone



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
                    Route.Profile { email = user.email, tab = Nothing }

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
    Request.post common "/members/profile" profile
        |> Task.attempt OnSubmit


registerNewMember : Encode.Value -> Common -> Cmd Msg
registerNewMember profile common =
    Request.post common "/members" profile
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
            if isJust model.common.user then
                "Edit Profile"

            else
                "Create Profile"
    in
    Basics.section
        [ Basics.container
            [ Basics.title titleText
            , Basics.box
                [ headerText <| isJust model.common.user
                , br [] []
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
    Basics.form Submit
        -- required fields
        [ Basics.title4 "Really Important Stuff"
        , inputWrapper [ Forms.Horizontal, Forms.Title "Name" ]
            [ textInput Forms.string
                { value = profileForm.firstName
                , onInput = \firstName -> EditForm { profileForm | firstName = firstName }
                , attrs = [ Forms.Placeholder "First", Forms.RequiredField True ]
                }
            , textInput Forms.string
                { value = profileForm.preferredName
                , onInput = \preferredName -> EditForm { profileForm | preferredName = preferredName }
                , attrs = [ Forms.Placeholder "Preferred (optional)" ]
                }
            , textInput Forms.string
                { value = profileForm.lastName
                , onInput = \lastName -> EditForm { profileForm | lastName = lastName }
                , attrs = [ Forms.Placeholder "Last", Forms.RequiredField True ]
                }
            ]
        , textInput Forms.email
            { value = profileForm.email
            , onInput = \email -> EditForm { profileForm | email = email }
            , attrs =
                [ Forms.Title "E-mail"
                , Forms.Horizontal
                , Forms.Placeholder "gburdell3@gatech.edu"
                , Forms.RequiredField True
                ]
            }
        , textInput Forms.string
            { value = profileForm.phoneNumber
            , onInput = \phoneNumber -> EditForm { profileForm | phoneNumber = phoneNumber }
            , attrs =
                [ Forms.Title "Phone Number"
                , Forms.Horizontal
                , Forms.Placeholder "6788675309"
                , Forms.RequiredField True
                ]
            }
        , inputWrapper [ Forms.Title "Password", Forms.Horizontal ]
            [ textInput Forms.password
                { value = profileForm.password
                , onInput = \password -> EditForm { profileForm | password = password }
                , attrs =
                    [ Forms.Placeholder "Password"
                    , Forms.RequiredField True
                    ]
                }
            , textInput Forms.password
                { value = profileForm.confirmPassword
                , onInput = \confirmPassword -> EditForm { profileForm | confirmPassword = confirmPassword }
                , attrs =
                    [ Forms.Placeholder "Confirm Password"
                    , Forms.RequiredField True
                    ]
                }
            ]
        , inputWrapper [ Forms.Title "Location", Forms.Horizontal ]
            (locationFieldBlock profileForm)
        , textInput Forms.string
            { value = profileForm.major
            , onInput = \major -> EditForm { profileForm | major = major }
            , attrs =
                [ Forms.Title "Major"
                , Forms.Horizontal
                , Forms.Placeholder "Undecided Engineering"
                , Forms.RequiredField True
                ]
            }
        , textInput Forms.string
            { value = profileForm.hometown
            , onInput = \hometown -> EditForm { profileForm | hometown = hometown }
            , attrs =
                [ Forms.Title "Hometown"
                , Forms.Horizontal
                , Forms.Placeholder "Winslow, Arizona"
                , Forms.RequiredField True
                ]
            }
        , inputWrapper [ Forms.Title "Car", Forms.Horizontal ]
            [ carFieldBlock profileForm ]
        , inputWrapper [ Forms.Title "Enrollment", Forms.Horizontal ]
            [ enrollmentBlock model.common profileForm ]

        -- optional fields
        , Basics.title4 "Nice to Know"
        , textInput Forms.string
            { value = profileForm.about
            , onInput = \about -> EditForm { profileForm | about = about }
            , attrs =
                [ Forms.Title "About"
                , Forms.Horizontal
                , Forms.Placeholder "I like big butts and I cannot lie"
                , Forms.RequiredField False
                ]
            }
        , textInput Forms.string
            { value = profileForm.about
            , onInput = \about -> EditForm { profileForm | about = about }
            , attrs =
                [ Forms.Title "About"
                , Forms.Horizontal
                , Forms.Placeholder "I like big butts and I cannot lie"
                , Forms.RequiredField False
                ]
            }
        , textInput Forms.string
            { value = profileForm.picture
            , onInput = \picture -> EditForm { profileForm | picture = picture }
            , attrs =
                [ Forms.Title "Picture URL"
                , Forms.Horizontal
                , Forms.Placeholder "https://create.mylittlepony.movie/images/ponyparticon_bodybig.png"
                , Forms.RequiredField False
                ]
            }
        , textInput Forms.int
            { value = Just profileForm.arrivedAtTech
            , onInput =
                \arrivedAtTech ->
                    EditForm
                        { profileForm
                            | arrivedAtTech = arrivedAtTech |> Maybe.withDefault (currentYear model.common)
                        }
            , attrs =
                [ Forms.Title "Arrived at Tech"
                , Forms.Horizontal
                , Forms.Placeholder "2099"
                , Forms.RequiredField False
                ]
            }
        , actionButtons (model.state == Sending) (model.common.user |> Maybe.map .email)
        ]


actionButtons : Bool -> Maybe String -> Html Msg
actionButtons sending maybeEmail =
    let
        backButton email =
            Buttons.link
                { content = "Back"
                , route = Route.Profile { email = email, tab = Nothing }
                , attrs = []
                }

        saveButton =
            Buttons.submit
                { content = "Save"
                , attrs =
                    [ Buttons.Color Buttons.IsPrimary
                    , Buttons.IsLoading sending
                    ]
                }
    in
    Buttons.group
        { alignment = Buttons.AlignRight
        , connected = False
        , buttons =
            [ maybeEmail |> Maybe.map backButton, Just saveButton ]
                |> List.filterMap identity
        }


carFieldBlock : ProfileForm -> Html Msg
carFieldBlock profileForm =
    let
        hasCarCheckbox =
            checkboxInput
                { content = "I have a car"
                , isChecked = profileForm.passengers > 0
                , onChange =
                    \hasCar ->
                        EditForm
                            { profileForm
                                | passengers =
                                    if hasCar then
                                        1

                                    else
                                        0
                            }
                }

        passengersField =
            textInput Forms.int
                { value = Just profileForm.passengers
                , onInput =
                    \passengers ->
                        EditForm
                            { profileForm
                                | passengers = passengers |> Maybe.withDefault 0
                            }
                , attrs =
                    [ Forms.Placeholder "How many?"
                    , Forms.Suffix "passengers"
                    ]
                }
    in
    div [ class "field is-grouped" ]
        ([ Just hasCarCheckbox
         , Just passengersField |> Maybe.filter (\_ -> profileForm.passengers > 0)
         ]
            |> List.filterMap identity
        )


locationFieldBlock : ProfileForm -> List (Html Msg)
locationFieldBlock profileForm =
    [ textInput Forms.string
        { value = profileForm.location
        , onInput = \location -> EditForm { profileForm | location = location }
        , attrs = [ Forms.Placeholder "Glenn" ]
        }
    , Forms.control
        [ Buttons.group
            { alignment = Buttons.AlignLeft
            , connected = True
            , buttons =
                [ Buttons.button
                    { content = "On-campus"
                    , onClick = Just <| EditForm { profileForm | onCampus = True }
                    , attrs =
                        [ Buttons.Color Buttons.IsPrimary ]
                            |> List.filter (\_ -> profileForm.onCampus)
                    }
                , Buttons.button
                    { content = "Off-campus"
                    , onClick = Just <| EditForm { profileForm | onCampus = False }
                    , attrs =
                        [ Buttons.Color Buttons.IsPrimary ]
                            |> List.filter (\_ -> not profileForm.onCampus)
                    }
                ]
            }
        ]
    ]


enrollmentBlock : Common -> ProfileForm -> Html Msg
enrollmentBlock common profileForm =
    Forms.inputWrapper [ Forms.Horizontal ]
        [ enrollmentOptions profileForm (isJust common.user)
        , span [ style "width" "15px" ] []
        , selectInput (Forms.section common)
            { values = common.info.sections |> List.map Just
            , selected = profileForm.section
            , onInput = \section -> EditForm { profileForm | section = section }
            , attrs = []
            }
        ]


enrollmentOptions : ProfileForm -> Bool -> Html Msg
enrollmentOptions profile isLoggedIn =
    let
        enrollmentOption enrollment =
            Buttons.button
                { content =
                    enrollment
                        |> Maybe.map enrollmentToString
                        |> Maybe.withDefault "Inactive"
                , onClick = Just <| EditForm { profile | enrollment = enrollment }
                , attrs =
                    [ Buttons.Color Buttons.IsPrimary ]
                        |> List.filter (\_ -> profile.enrollment == enrollment)
                }
    in
    Forms.control
        [ Buttons.group
            { alignment = Buttons.AlignLeft
            , connected = True
            , buttons =
                [ Just (enrollmentOption Nothing)
                    |> Maybe.filter (\_ -> isLoggedIn)
                , Just (enrollmentOption (Just Class))
                , Just (enrollmentOption (Just Club))
                ]
                    |> List.filterMap identity
            }
        ]
