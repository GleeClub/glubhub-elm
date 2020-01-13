module Page.ForgotPassword exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Forms exposing (emailInput)
import Error exposing (GreaseError(..), GreaseResult)
import Html exposing (Html, br, button, div, form, h4, p, text)
import Html.Attributes exposing (class, style, type_)
import Html.Events exposing (onSubmit)
import Http.Detailed exposing (Error(..))
import Json.Encode as Encode
import Route
import Task
import Utils exposing (RemoteData(..), SubmissionState(..), alert, isLoadingClass, postRequest)



---- MODEL ----


type alias Model =
    { email : String
    , confirmEmail : String
    , state : SubmissionState
    }


init : ( Model, Cmd Msg )
init =
    ( { email = ""
      , confirmEmail = ""
      , state = NotSentYet
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateEmail String
    | UpdateConfirmEmail String
    | Submit
    | OnSubmitForgotPassword (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmail email ->
            ( { model | email = email }, Cmd.none )

        UpdateConfirmEmail confirmEmail ->
            ( { model | confirmEmail = confirmEmail }, Cmd.none )

        Submit ->
            if model.email /= model.confirmEmail then
                ( model, alert "Your emails don't match." )

            else
                ( { model | state = Sending }, requestPasswordReset model )

        OnSubmitForgotPassword (Ok _) ->
            ( model
            , Cmd.batch
                [ Route.loadPage Route.Login
                , alert "Check your email for a password reset link, it should be there in a few minutes."
                ]
            )

        OnSubmitForgotPassword (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )



---- DATA ----


requestPasswordReset : Model -> Cmd Msg
requestPasswordReset model =
    let
        url =
            "/forgot_password/" ++ model.email
    in
    postRequest { token = "" } url (Encode.object [])
        |> Task.attempt OnSubmitForgotPassword



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container fullheight" ]
        [ div [ class "columns is-centered is-vcentered" ]
            [ Basics.narrowColumn
                [ form [ onSubmit Submit, style "padding" "10px" ]
                    [ Basics.box
                        [ h4 [ class "title" ] [ text "Forgot your password?" ]
                        , p []
                            [ text "That sucks. But don't \"oh geez, oh frick\", just slap "
                            , text "some emails down and we will send you an email with a reset link."
                            ]
                        , br [] []
                        , emailInput
                            { title = "E-mail"
                            , helpText = Nothing
                            , value = model.email
                            , placeholder = "gburdell3@gatech.edu"
                            , required = True
                            , onInput = UpdateEmail
                            }
                        , emailInput
                            { title = "Confirm E-mail"
                            , helpText = Nothing
                            , value = model.confirmEmail
                            , placeholder = "bgurdell3@gatech.edu"
                            , required = True
                            , onInput = UpdateConfirmEmail
                            }
                        , actionButtons model
                        , case model.state of
                            ErrorSending error ->
                                Basics.errorBox error

                            _ ->
                                text ""
                        ]
                    ]
                ]
            ]
        ]


actionButtons : Model -> Html Msg
actionButtons model =
    div [ class "buttons is-right" ]
        [ Basics.linkButton "uh, nvm" Route.Login
        , button
            [ type_ "submit"
            , class <| "button is-primary" ++ isLoadingClass (model.state == Sending)
            ]
            [ text "halp" ]
        ]
