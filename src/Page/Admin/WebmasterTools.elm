module Page.Admin.WebmasterTools exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Forms exposing (fileInput)
import Error exposing (GreaseResult, parseResponse)
import File exposing (File)
import Html exposing (Html, button, div, header, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), alert, isLoadingClass)



---- MODEL ----


type alias Model =
    { common : Common
    , apiFile : Maybe File
    , apiState : SubmissionState
    , frontendFile : Maybe File
    , frontendState : SubmissionState
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , apiFile = Nothing
      , apiState = NotSentYet
      , frontendFile = Nothing
      , frontendState = NotSentYet
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SelectApiBinary (List File)
    | UploadApi
    | OnUploadApi (GreaseResult ())
    | SelectFrontendZip (List File)
    | UploadFrontend
    | OnUploadFrontend (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectApiBinary files ->
            ( { model | apiFile = List.head files }, Cmd.none )

        UploadApi ->
            case model.apiFile of
                Nothing ->
                    ( model, alert "fam. upload a file." )

                Just file ->
                    ( { model | apiState = Sending }, uploadApi model.common file )

        OnUploadApi (Ok _) ->
            ( { model | apiState = NotSentYet, apiFile = Nothing }, Cmd.none )

        OnUploadApi (Err error) ->
            ( { model | apiState = ErrorSending error }, Cmd.none )

        SelectFrontendZip files ->
            ( { model | frontendFile = List.head files }, Cmd.none )

        UploadFrontend ->
            case model.frontendFile of
                Nothing ->
                    ( model, alert "fam. upload a file." )

                Just file ->
                    ( { model | frontendState = Sending }, uploadFrontend model.common file )

        OnUploadFrontend (Ok _) ->
            ( { model | frontendState = NotSentYet, frontendFile = Nothing }, Cmd.none )

        OnUploadFrontend (Err error) ->
            ( { model | frontendState = ErrorSending error }, Cmd.none )



---- DATA ----


uploadApi : Common -> File -> Cmd Msg
uploadApi common apiFile =
    Http.task
        { method = "POST"
        , url = "https://gleeclub.gatech.edu/cgi-bin/admin_tools/upload_api"
        , body = Http.fileBody apiFile
        , headers = [ Http.header "token" common.token ]
        , timeout = Nothing
        , resolver = Http.stringResolver <| parseResponse (Decode.succeed ())
        }
        |> Task.attempt OnUploadApi


uploadFrontend : Common -> File -> Cmd Msg
uploadFrontend common frontendFile =
    Http.task
        { method = "POST"
        , url = "https://gleeclub.gatech.edu/cgi-bin/api/upload_frontend"
        , body = Http.fileBody frontendFile
        , headers = [ Http.header "token" common.token ]
        , timeout = Nothing
        , resolver = Http.stringResolver <| parseResponse (Decode.succeed ())
        }
        |> Task.attempt OnUploadFrontend



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Webmaster Tools"
        , Basics.box
            [ Basics.columns
                [ Basics.column
                    [ fileInput
                        { title = "Upload the API:"
                        , helpText = Nothing
                        , file = model.apiFile
                        , selectFile = SelectApiBinary
                        }
                    , button
                        [ class <| "button" ++ isLoadingClass (model.apiState == Sending)
                        , onClick UploadApi
                        ]
                        [ text "Send it!" ]
                    , case model.apiState of
                        ErrorSending error ->
                            Basics.errorBox error

                        _ ->
                            text ""
                    ]
                , div [ class "is-divider-vertical" ] []
                , Basics.column
                    [ fileInput
                        { title = "Upload the frontend:"
                        , helpText = Nothing
                        , file = model.frontendFile
                        , selectFile = SelectFrontendZip
                        }
                    , button
                        [ class <| "button" ++ isLoadingClass (model.frontendState == Sending)
                        , onClick UploadFrontend
                        ]
                        [ text "Send it!" ]
                    , case model.frontendState of
                        ErrorSending error ->
                            Basics.errorBox error

                        _ ->
                            text ""
                    ]
                ]
            ]
        ]
