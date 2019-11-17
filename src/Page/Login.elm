module Page.Login exposing (LoginRequestState(..), Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Html exposing (Html, a, button, div, form, h1, img, input, label, section, span, text)
import Html.Attributes exposing (class, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Metadata)
import Http.Detailed exposing (Error(..))
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import MD5
import Models.Event exposing (Member, memberDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), alert, apiUrl, notFoundView, setToken, spinner)



---- MODEL ----


type alias Model =
    { email : String
    , password : String
    , state : LoginRequestState
    }


type LoginRequestState
    = NotSentYet
    | Sending
    | Error String


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
    | OnSubmitLogin (Result (Http.Detailed.Error String) ( Metadata, String ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmail email ->
            ( { model | email = email }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        Submit ->
            ( { model | state = Sending }, submitLogin model )

        OnSubmitLogin (Ok ( _, token )) ->
            ( model, onSuccessfulLogin token )

        OnSubmitLogin (Err error) ->
            case error of
                BadStatus _ body ->
                    case body |> Decode.decodeString (field "token" string) of
                        Ok token ->
                            ( model, onSuccessfulLogin token )

                        Err _ ->
                            ( { model | state = Error "an error occurred" }, alert body )

                _ ->
                    ( { model | state = Error "an error occurred" }, Cmd.none )



---- DATA ----


submitLogin : Model -> Cmd Msg
submitLogin model =
    let
        passHash =
            MD5.hex model.password

        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "email", Encode.string model.email )
                    , ( "passHash", Encode.string passHash )
                    ]
    in
    Http.request
        { method = "POST"
        , url = apiUrl ++ "/login"
        , body = body
        , headers = []
        , expect = Http.Detailed.expectJson OnSubmitLogin (field "token" string)
        , timeout = Nothing
        , tracker = Nothing
        }


onSuccessfulLogin : String -> Cmd Msg
onSuccessfulLogin token =
    Cmd.batch
        [ setToken (Just token)
        , Route.loadPage Route.Home
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container fullheight" ]
        [ div [ class "columns is-centered is-vcentered", style "display" "flex" ]
            [ Basics.narrowColumn
                [ form [ id "login", class "box", onSubmit Submit ]
                    [ logo
                    , emailField model
                    , passwordField model
                    , actionButtons model
                    ]
                ]
            ]
        ]


logo : Html Msg
logo =
    h1 [ class "title is-1", style "text-align" "center" ]
        [ span [ style "color" "#b4a46a" ] [ text "Glub" ]
        , span [ style "color" "#666" ] [ text "Hub" ]
        ]


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
            , class "button is-primary"
            , class <|
                case model.state of
                    Sending ->
                        " is-loading"

                    _ ->
                        ""
            ]
            [ text "Sign In" ]
        ]
