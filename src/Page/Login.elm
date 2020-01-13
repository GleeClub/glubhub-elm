module Page.Login exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Error exposing (GreaseError(..), GreaseResult)
import Html exposing (Html, button, div, form, img, text)
import Html.Attributes exposing (class, src, style, type_)
import Html.Events exposing (onSubmit)
import Http.Detailed exposing (Error(..))
import Json.Decode exposing (field, string)
import Json.Encode as Encode
import MD5
import Route
import Task
import Utils exposing (RemoteData(..), SubmissionState(..), alert, isLoadingClass, postRequestFull, setToken)



---- MODEL ----


type alias Model =
    { email : String
    , password : String
    , state : SubmissionState
    }


init : ( Model, Cmd Msg )
init =
    ( { email = ""
      , password = ""
      , state = NotSentYet
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | Submit
    | OnSubmitLogin (GreaseResult String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmail email ->
            ( { model | email = email }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        Submit ->
            ( { model | state = Sending }, submitLogin model )

        OnSubmitLogin (Ok token) ->
            ( model, onSuccessfulLogin token )

        OnSubmitLogin (Err error) ->
            case error of
                AlreadyLoggedIn token ->
                    ( model, onSuccessfulLogin token )

                otherError ->
                    ( { model | state = ErrorSending otherError }
                    , alert "Your username and/or password were incorrect."
                    )



---- DATA ----


submitLogin : Model -> Cmd Msg
submitLogin model =
    let
        passHash =
            MD5.hex model.password

        loginJson =
            Encode.object
                [ ( "email", Encode.string model.email )
                , ( "passHash", Encode.string passHash )
                ]
    in
    postRequestFull { token = "" } "/login" loginJson (field "token" string)
        |> Task.attempt OnSubmitLogin


onSuccessfulLogin : String -> Cmd Msg
onSuccessfulLogin token =
    Cmd.batch [ setToken (Just token), Nav.reload ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container fullheight" ]
        [ div [ class "columns is-centered is-vcentered", style "display" "flex" ]
            [ form [ onSubmit Submit ]
                [ Basics.narrowColumn
                    [ Basics.box
                        [ logo
                        , emailField model
                        , passwordField model
                        , actionButtons model
                        ]
                    ]
                ]
            ]
        ]


logo : Html Msg
logo =
    img [ style "width" "100%", src "./glubhub.svg" ] []


emailField : Model -> Html Msg
emailField model =
    Basics.horizontalField
        { label = "E-mail"
        , name = "login-email"
        , type_ = "email"
        , value = model.email
        , onInput = UpdateEmail
        , placeholder = "gburdell3@gatech.edu"
        }


passwordField : Model -> Html Msg
passwordField model =
    Basics.horizontalField
        { label = "Password"
        , name = "password"
        , type_ = "password"
        , value = model.password
        , onInput = UpdatePassword
        , placeholder = "••••••••"
        }


actionButtons : Model -> Html Msg
actionButtons model =
    div [ class "buttons is-right" ]
        [ Basics.linkButton "Register" Route.EditProfile
        , Basics.linkButton "Forgot" Route.ForgotPassword
        , button
            [ type_ "submit"
            , class <| "button is-primary" ++ isLoadingClass (model.state == Sending)
            ]
            [ text "Sign In" ]
        ]
