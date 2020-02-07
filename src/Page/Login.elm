module Page.Login exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.Forms as Forms exposing (textInput)
import Error exposing (GreaseError(..), GreaseResult)
import Html exposing (Html, br, div, form, img)
import Html.Attributes exposing (class, src, style)
import Http.Detailed exposing (Error(..))
import Json.Decode exposing (field, string)
import Json.Encode as Encode
import MD5
import Request
import Route
import Task
import Utils exposing (RemoteData(..), SubmissionState(..), alert, setToken)



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
    Request.postFull { token = "" } "/login" loginJson (field "token" string)
        |> Task.attempt OnSubmitLogin


onSuccessfulLogin : String -> Cmd Msg
onSuccessfulLogin token =
    Cmd.batch [ setToken (Just token), Nav.reload ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container fullheight" ]
        [ div
            [ class "columns is-centered is-vcentered"
            , style "display" "flex"
            ]
            [ Basics.narrowColumn
                [ Basics.box
                    [ Basics.form Submit
                        [ logo
                        , emailField model
                        , passwordField model
                        , div [] (actionButtons model)
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
    textInput Forms.email
        { value = model.email
        , onInput = UpdateEmail
        , attrs =
            [ Forms.Title "Who are you?"
            , Forms.Placeholder "gburdell3@gatech.edu"
            ]
        }


passwordField : Model -> Html Msg
passwordField model =
    textInput Forms.password
        { value = model.password
        , onInput = UpdatePassword
        , attrs =
            [ Forms.Title "Oh yeah? Prove it."
            , Forms.Placeholder "••••••••"
            ]
        }


actionButtons : Model -> List (Html Msg)
actionButtons model =
    [ Buttons.submit
        { content = "I posit that I am worthy"
        , attrs =
            [ Buttons.Color Buttons.IsPrimary
            , Buttons.IsLoading (model.state == Sending)
            , Buttons.CustomAttrs [ class "is-fullwidth" ]
            ]
        }
    , br [] []
    , div [ class "field is-grouped is-grouped-centered is-expanded" ]
        [ div [ class "control" ]
            [ Buttons.link
                { content = "I have forgotten who I am"
                , route = Route.ForgotPassword
                , attrs = []
                }
            ]
        , div [ class "control" ]
            [ Buttons.link
                { content = "I am not anyone yet"
                , route = Route.EditProfile
                , attrs =
                    [ Buttons.IsOutlined
                    , Buttons.Color Buttons.IsPrimary
                    ]
                }
            ]
        ]
    ]
