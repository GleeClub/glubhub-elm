module Page.Admin.Uniforms exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, footer, form, h1, header, i, img, input, label, p, section, span, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (attribute, class, colspan, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import List.Extra exposing (getAt, groupWhile, setAt, updateAt)
import Models.Event exposing (EventAttendee, Member, eventAttendeeDecoder)
import Models.Info exposing (Uniform, uniformDecoder)
import Route exposing (Route)
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), alert, deleteRequest, getRequest, mapLoaded, postRequest, postRequestFull, remoteToMaybe, resultToRemote, resultToSubmissionState)



---- MODEL ----


type alias Model =
    { common : Common
    , uniforms : RemoteData (List Uniform)
    , newUniform : Uniform
    , state : SubmissionState
    , tryingToDelete : Maybe Int
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , uniforms = Loading
      , newUniform = emptyUniform
      , state = NotSentYet
      , tryingToDelete = Nothing
      }
    , loadUniforms common
    )


emptyUniform : Uniform
emptyUniform =
    { id = 0
    , name = ""
    , description = Just ""
    , color = Nothing
    }


editLinksPermission : String
editLinksPermission =
    "edit-links"



---- UPDATE ----


type Msg
    = OnLoadUniforms (GreaseResult (List Uniform))
    | OnChangeUniform (GreaseResult ())
    | UpdateUniform Uniform Int
    | SendUniformUpdate Int
    | TryToDeleteUniform Int
    | CancelDeleteUniform
    | ConfirmDeleteUniform
    | InputNewName String
    | InputNewDescription String
    | CreateNewUniform
    | OnCreateUniform (GreaseResult Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadUniforms uniformsResult ->
            ( { model | uniforms = resultToRemote uniformsResult }, Cmd.none )

        OnChangeUniform result ->
            ( { model | state = resultToSubmissionState result }, Cmd.none )

        UpdateUniform uniform uniformIndex ->
            ( { model | uniforms = model.uniforms |> mapLoaded (setAt uniformIndex uniform) }, Cmd.none )

        SendUniformUpdate index ->
            case model |> findUniform index of
                Just uniform ->
                    ( { model | state = Sending }, updateUniform model.common uniform )

                Nothing ->
                    ( model, Cmd.none )

        TryToDeleteUniform index ->
            ( { model | tryingToDelete = Just index }, Cmd.none )

        CancelDeleteUniform ->
            ( { model | tryingToDelete = Nothing }, Cmd.none )

        ConfirmDeleteUniform ->
            case ( model.tryingToDelete, model.tryingToDelete |> Maybe.andThen (\index -> model |> findUniform index), model.uniforms ) of
                ( Just index, Just uniform, Loaded uniforms ) ->
                    ( { model
                        | uniforms = Loaded (uniforms |> List.Extra.removeAt index)
                        , state = Sending
                        , tryingToDelete = Nothing
                      }
                    , deleteUniform model.common uniform
                    )

                _ ->
                    ( model, Cmd.none )

        InputNewName name ->
            let
                newUniform =
                    model.newUniform
            in
            ( { model | newUniform = { newUniform | name = name } }, Cmd.none )

        InputNewDescription description ->
            let
                newUniform =
                    model.newUniform
            in
            ( { model | newUniform = { newUniform | description = Just description } }, Cmd.none )

        CreateNewUniform ->
            ( { model
                | newUniform = emptyUniform
                , uniforms = model.uniforms |> mapLoaded (\uniforms -> uniforms ++ [ model.newUniform ])
                , state = Sending
              }
            , createUniform model.common model.newUniform
            )

        OnCreateUniform (Ok newId) ->
            let
                updateUniforms uniforms =
                    uniforms
                        |> updateAt
                            (List.length uniforms - 1)
                            (\uniform -> { uniform | id = newId })
            in
            ( { model | uniforms = model.uniforms |> mapLoaded updateUniforms, state = NotSentYet }
            , Cmd.none
            )

        OnCreateUniform (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )



---- DATA ----


findUniform : Int -> Model -> Maybe Uniform
findUniform index model =
    model.uniforms |> remoteToMaybe |> Maybe.andThen (List.Extra.getAt index)


loadUniforms : Common -> Cmd Msg
loadUniforms common =
    getRequest common "/uniforms" (Decode.list uniformDecoder)
        |> Task.attempt OnLoadUniforms


updateUniform : Common -> Uniform -> Cmd Msg
updateUniform common uniform =
    let
        url =
            "/uniforms/" ++ String.fromInt uniform.id

        body =
            uniform |> serializeUniform
    in
    postRequest common url body
        |> Task.attempt OnChangeUniform


deleteUniform : Common -> Uniform -> Cmd Msg
deleteUniform common uniform =
    deleteRequest common ("/uniforms/" ++ String.fromInt uniform.id)
        |> Task.attempt OnChangeUniform


createUniform : Common -> Uniform -> Cmd Msg
createUniform common uniform =
    postRequestFull common "/uniforms" (serializeUniform uniform) (field "id" Decode.int)
        |> Task.attempt OnCreateUniform


serializeUniform : Uniform -> Encode.Value
serializeUniform uniform =
    Encode.object
        [ ( "name", Encode.string uniform.name )
        , ( "description", Encode.string (uniform.description |> Maybe.withDefault "") )
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Uniforms"
        , Basics.box
            [ model.uniforms |> Basics.remoteContent (uniformTable model.newUniform)
            , deleteUniformModal model
            , Basics.submissionStateBox model.state
            ]
        ]


uniformTable : Uniform -> List Uniform -> Html Msg
uniformTable newUniform allUniforms =
    let
        uniformRows =
            allUniforms |> List.indexedMap uniformRow

        newHeaderRow =
            tr [] [ td [] [ text "New" ] ]
    in
    table [ style "border-spacing" "5px", style "border-collapse" "separate" ]
        ((headerRow :: uniformRows) ++ [ newHeaderRow, newUniformRow newUniform ])


headerRow : Html Msg
headerRow =
    tr []
        [ th [] [ b [] [ text "Name" ] ]
        , th [] [ b [] [ text "Description" ] ]
        ]


uniformRow : Int -> Uniform -> Html Msg
uniformRow index uniform =
    tr []
        [ td [ style "padding-right" "10px" ]
            [ input
                [ type_ "text"
                , class "input"
                , value uniform.name
                , placeholder "Name"
                , onInput (\name -> UpdateUniform { uniform | name = name } index)
                , onBlur (SendUniformUpdate index)
                ]
                []
            ]
        , td []
            [ textarea
                [ class "textarea"
                , value (uniform.description |> Maybe.withDefault "")
                , placeholder "Description"
                , onInput (\description -> UpdateUniform { uniform | description = Just description } index)
                , onBlur (SendUniformUpdate index)
                ]
                []
            ]
        , td []
            [ span [ style "display" "inline-block", style "vertical-align" "middle" ]
                [ button
                    [ class "delete"
                    , attribute "aria-label" "delete"
                    , onClick (TryToDeleteUniform index)
                    ]
                    []
                ]
            ]
        ]


newUniformRow : Uniform -> Html Msg
newUniformRow uniform =
    tr []
        [ td []
            [ input
                [ type_ "text"
                , class "input"
                , value uniform.name
                , placeholder "Name"
                , onInput InputNewName
                ]
                []
            ]
        , td []
            [ textarea
                [ class "textarea"
                , value (uniform.description |> Maybe.withDefault "")
                , placeholder "Description"
                , onInput InputNewDescription
                ]
                []
            ]
        , td []
            [ button [ class "button is-primary", onClick CreateNewUniform ]
                [ text "Suit up." ]
            ]
        ]


deleteUniformModal : Model -> Html Msg
deleteUniformModal model =
    case
        model.tryingToDelete
            |> Maybe.andThen
                (\index ->
                    model.uniforms
                        |> remoteToMaybe
                        |> Maybe.andThen (getAt index)
                )
    of
        Nothing ->
            div [] []

        Just uniform ->
            div [ class "modal is-active" ]
                [ div [ class "modal-background", onClick CancelDeleteUniform ] []
                , div [ class "modal-card" ]
                    [ modalHeader uniform, modalBody uniform, modalButtons ]
                ]


modalHeader : Uniform -> Html Msg
modalHeader uniform =
    header [ class "modal-card-head" ]
        [ p [ class "modal-card-title" ]
            [ text <| "Delete uniform " ++ uniform.name ++ "?" ]
        , button
            [ class "delete"
            , attribute "aria-label" "close"
            , onClick CancelDeleteUniform
            ]
            []
        ]


modalBody : Uniform -> Html Msg
modalBody uniform =
    section [ class "modal-card-body" ]
        [ p [] [ text <| "Are you sure you want to delete the " ++ uniform.name ++ " uniform?" ]
        , p [] [ i [] [ text "Note: all events that have this uniform will no longer have a uniform." ] ]
        ]


modalButtons : Html Msg
modalButtons =
    footer [ class "modal-card-foot" ]
        [ button
            [ class "button is-danger"
            , onClick ConfirmDeleteUniform
            ]
            [ text "Delete" ]
        , button
            [ class "button"
            , onClick CancelDeleteUniform
            ]
            [ text "Cancel" ]
        ]
