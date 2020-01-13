module Page.ResetPassword exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Forms exposing (passwordInput)
import Error exposing (GreaseError(..), GreaseResult)
import Html exposing (Html, br, button, div, form, h4, p, text)
import Html.Attributes exposing (class, style, type_)
import Html.Events exposing (onSubmit)
import Http.Detailed exposing (Error(..))
import Json.Encode as Encode
import MD5
import Route
import Task
import Utils exposing (RemoteData(..), SubmissionState(..), alert, isLoadingClass, postRequest)



---- MODEL ----


type alias Model =
    { token : Maybe String
    , password : String
    , confirmPassword : String
    , state : SubmissionState
    }


init : Maybe String -> ( Model, Cmd Msg )
init token =
    ( { token = token
      , password = ""
      , confirmPassword = ""
      , state = NotSentYet
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdatePassword String
    | UpdateConfirmPassword String
    | Submit
    | OnResetPassword (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        UpdateConfirmPassword confirmPassword ->
            ( { model | confirmPassword = confirmPassword }, Cmd.none )

        Submit ->
            if model.password /= model.confirmPassword then
                ( model, alert "Your passwords don't match." )

            else
                ( { model | state = Sending }, resetPassword model )

        OnResetPassword (Ok _) ->
            ( model
            , Cmd.batch
                [ Route.loadPage Route.Login
                , alert "Your password has been successfully reset!"
                ]
            )

        OnResetPassword (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )



---- DATA ----


resetPassword : Model -> Cmd Msg
resetPassword model =
    let
        passHash =
            MD5.hex model.password

        body =
            Encode.object
                [ ( "passHash", Encode.string passHash )
                ]

        url =
            "/reset_password"
                ++ (model.token
                        |> Maybe.map (\token -> "?token=" ++ token)
                        |> Maybe.withDefault ""
                   )
    in
    postRequest { token = "" } url body
        |> Task.attempt OnResetPassword



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container fullheight" ]
        [ div [ class "columns is-centered is-vcentered" ]
            [ Basics.narrowColumn
                [ form [ onSubmit Submit, style "padding" "10px" ]
                    [ Basics.box
                        [ h4 [ class "title" ] [ text "Reset Your Password" ]
                        , p []
                            [ text "Good job getting this far. Gimme a new password, "
                            , text "and you'll be reborn like it's Avatar 2009."
                            ]
                        , br [] []
                        , passwordInput
                            { title = "Password"
                            , helpText = Nothing
                            , value = model.password
                            , placeholder = "••••••••"
                            , required = True
                            , onInput = UpdatePassword
                            }
                        , passwordInput
                            { title = "Confirm Password"
                            , helpText = Nothing
                            , value = model.confirmPassword
                            , placeholder = "••••••••"
                            , required = True
                            , onInput = UpdateConfirmPassword
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
        [ button
            [ type_ "submit"
            , class <| "button is-primary" ++ isLoadingClass (model.state == Sending)
            ]
            [ text "call me Jake Sully" ]
        ]
