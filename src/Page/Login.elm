module Page.Login exposing (LoginRequestState(..), Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, img, input, label, section, span, text)
import Html.Attributes exposing (class, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import MD5
import Models.Member exposing (Member, memberDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), apiUrl, notFoundView, setToken, spinner)



---- MODEL ----


type alias Model =
    { email : String
    , password : String
    , state : LoginRequestState
    , key : Nav.Key
    }


type LoginRequestState
    = NotSentYet
    | Sending
    | Error String


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { email = ""
      , password = ""
      , state = NotSentYet
      , key = key
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | Submit
    | OnSubmitLogin (Result Http.Error String)


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
            ( model, onSuccessfulLogin token model.key )

        OnSubmitLogin (Err error) ->
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
        , expect = Http.expectJson OnSubmitLogin (field "token" string)
        , timeout = Nothing
        , tracker = Nothing
        }


onSuccessfulLogin : String -> Nav.Key -> Cmd Msg
onSuccessfulLogin token key =
    Cmd.batch
        [ setToken (Just token)
        , Route.replaceUrl key Route.Home
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container fullheight" ]
        [ div [ class "columns is-centered is-vcentered", style "display" "flex" ]
            [ div [ class "column is-narrow" ]
                [ form [ id "login", class "box", onSubmit Submit ]
                    [ h1 [ class "title is-1", style "text-align" "center" ]
                        [ span [ style "color" "#b4a46a" ] [ text "Glub" ]
                        , span [ style "color" "#666" ] [ text "Hub" ]
                        ]
                    , emailField model
                    , passwordField model
                    , actionButtons model
                    ]
                ]
            ]
        ]


emailField : Model -> Html Msg
emailField model =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ] [ text "E-mail" ] ]
        , div [ class "control" ]
            [ input
                [ class "input"
                , type_ "email"
                , value model.email
                , onInput UpdateEmail
                , placeholder "gburdell3@gatech.edu"
                ]
                []
            ]
        ]


passwordField : Model -> Html Msg
passwordField model =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ] [ text "Password" ] ]
        , div [ class "control" ]
            [ input
                [ class "input"
                , type_ "password"
                , value model.password
                , onInput UpdatePassword
                , placeholder "••••••••"
                ]
                []
            ]
        ]


actionButtons : Model -> Html Msg
actionButtons model =
    div [ class "buttons is-right" ]
        [ button [ type_ "button", class "button", Route.href Route.Home ] [ text "Register" ]
        , button [ type_ "button", class "button", Route.href Route.Home ] [ text "Forgot" ]
        , button
            [ type_ "submit"
            , class "button is-primary"
            , class <|
                case model.state of
                    Sending ->
                        " is-loading"

                    _ ->
                        ""
            , onClick Submit
            ]
            [ text "Sign In" ]
        ]
