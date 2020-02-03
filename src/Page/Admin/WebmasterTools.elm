module Page.Admin.WebmasterTools exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.Forms as Forms exposing (fileInput)
import Error exposing (GreaseResult, parseResponse)
import File exposing (File)
import Html exposing (Html, button, div, header, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), alert)



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
    = SelectApiBinary (Maybe File)
    | UploadApi
    | OnUploadApi (GreaseResult ())
    | SelectFrontendZip (Maybe File)
    | UploadFrontend
    | OnUploadFrontend (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectApiBinary file ->
            ( { model | apiFile = file }, Cmd.none )

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

        SelectFrontendZip file ->
            ( { model | frontendFile = file }, Cmd.none )

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
                        { file = model.apiFile
                        , selectFile = SelectApiBinary
                        , attrs = [ Forms.Title "Upload the API:" ]
                        }
                    , Buttons.button
                        { content = "Send it!"
                        , onClick = Just UploadApi
                        , attrs = [ Buttons.IsLoading (model.apiState == Sending) ]
                        }
                    , case model.apiState of
                        ErrorSending error ->
                            Basics.errorBox error

                        _ ->
                            text ""
                    ]
                , div [ class "is-divider-vertical" ] []
                , Basics.column
                    [ fileInput
                        { file = model.frontendFile
                        , selectFile = SelectFrontendZip
                        , attrs = [ Forms.Title "Upload the frontend:" ]
                        }
                    , Buttons.button
                        { content = "Send it!"
                        , onClick = Just UploadFrontend
                        , attrs = [ Buttons.IsLoading (model.frontendState == Sending) ]
                        }
                    , case model.frontendState of
                        ErrorSending error ->
                            Basics.errorBox error

                        _ ->
                            text ""
                    ]
                ]
            ]
        ]
