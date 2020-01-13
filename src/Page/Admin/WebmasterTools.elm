module Page.Admin.WebmasterTools exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Forms exposing (fileInput)
import File exposing (File)
import Html exposing (Html, button, div, header, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Utils exposing (Common, RemoteData(..), alert)



---- MODEL ----


type alias Model =
    { common : Common
    , apiFile : Maybe File
    , frontendFile : Maybe File
    , sending : Bool
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , apiFile = Nothing
      , frontendFile = Nothing
      , sending = False
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SelectApiBinary (List File)
    | UploadApi
    | OnUploadApi (Result Http.Error ())
    | SelectFrontendZip (List File)
    | UploadFrontend
    | OnUploadFrontend (Result Http.Error ())


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
                    ( { model | sending = True }, uploadApi model.common file )

        OnUploadApi (Ok _) ->
            ( { model | sending = False, apiFile = Nothing }, alert "Success!" )

        OnUploadApi (Err _) ->
            ( { model | sending = False }, alert "It broke." )

        SelectFrontendZip files ->
            ( { model | frontendFile = List.head files }, Cmd.none )

        UploadFrontend ->
            case model.frontendFile of
                Nothing ->
                    ( model, alert "fam. upload a file." )

                Just file ->
                    ( { model | sending = True }, uploadFrontend model.common file )

        OnUploadFrontend (Ok _) ->
            ( { model | sending = False, frontendFile = Nothing }, alert "Success!" )

        OnUploadFrontend (Err _) ->
            ( { model | sending = False }, alert "It broke." )



---- DATA ----


uploadApi : Common -> File -> Cmd Msg
uploadApi common apiFile =
    Http.request
        { method = "POST"
        , url = "https://gleeclub.gatech.edu/cgi-bin/admin_tools/upload_api"
        , body = Http.fileBody apiFile
        , headers = [ Http.header "token" common.token ]
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectWhatever OnUploadApi
        }


uploadFrontend : Common -> File -> Cmd Msg
uploadFrontend common frontendFile =
    Http.request
        { method = "POST"
        , url = "https://gleeclub.gatech.edu/cgi-bin/api/upload_frontend"
        , body = Http.fileBody frontendFile
        , headers = [ Http.header "token" common.token ]
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectWhatever OnUploadFrontend
        }



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
                    , button [ class "button", onClick UploadApi ]
                        [ text "Send it!" ]
                    ]
                , div [ class "is-divider-vertical" ] []
                , Basics.column
                    [ fileInput
                        { title = "Upload the frontend:"
                        , helpText = Nothing
                        , file = model.frontendFile
                        , selectFile = SelectFrontendZip
                        }
                    , button [ class "button", onClick UploadFrontend ]
                        [ text "Send it!" ]
                    ]
                ]
            ]
        ]
