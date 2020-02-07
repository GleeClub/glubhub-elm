module Page.Admin.Uniforms exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.DeleteModal exposing (deleteModal)
import Components.Forms as Forms exposing (textInput, textareaInput)
import Error exposing (GreaseResult)
import Html exposing (Html, b, button, div, i, p, span, table, td, text, th, tr)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (string)
import Json.Encode as Encode
import List.Extra exposing (getAt, setAt, updateAt)
import Models.Info exposing (Uniform, uniformDecoder)
import Request
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), mapLoaded, remoteToMaybe, resultToRemote, resultToSubmissionState)



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



---- UPDATE ----


type Msg
    = OnLoadUniforms (GreaseResult (List Uniform))
    | OnChangeUniform (GreaseResult ())
    | UpdateUniform Uniform Int
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
            ( { model | uniforms = model.uniforms |> mapLoaded (setAt uniformIndex uniform), state = Sending }
            , updateUniform model.common uniform
            )

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
    Request.get common "/uniforms" (Decode.list uniformDecoder)
        |> Task.attempt OnLoadUniforms


updateUniform : Common -> Uniform -> Cmd Msg
updateUniform common uniform =
    let
        url =
            "/uniforms/" ++ String.fromInt uniform.id

        body =
            uniform |> serializeUniform
    in
    Request.post common url body
        |> Task.attempt OnChangeUniform


deleteUniform : Common -> Uniform -> Cmd Msg
deleteUniform common uniform =
    Request.delete common ("/uniforms/" ++ String.fromInt uniform.id)
        |> Task.attempt OnChangeUniform


createUniform : Common -> Uniform -> Cmd Msg
createUniform common uniform =
    Request.postReturningId common "/uniforms" (serializeUniform uniform)
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
    div [ style "width" "100%" ]
        [ Basics.title "Uniforms"
        , Basics.box
            [ model.uniforms |> Basics.remoteContent (uniformTable model.newUniform)
            , Basics.submissionStateBox model.state
            , deleteUniformModal model
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
    table
        [ style "width" "100%"
        , style "border-spacing" "5px"
        , style "border-collapse" "separate"
        ]
        ((headerRow :: uniformRows) ++ [ newHeaderRow, newUniformRow newUniform ])


headerRow : Html Msg
headerRow =
    let
        columnNames =
            [ "Name", "Description" ]
    in
    tr []
        (columnNames
            |> List.map
                (\n ->
                    th []
                        [ b [] [ text n ] ]
                )
        )


uniformRow : Int -> Uniform -> Html Msg
uniformRow index uniform =
    let
        columns =
            [ textInput Forms.string
                { value = uniform.name
                , onInput = \name -> UpdateUniform { uniform | name = name } index
                , attrs = [ Forms.Placeholder "Name" ]
                }
            , textareaInput
                { value = uniform.description |> Maybe.withDefault ""
                , onInput = \description -> UpdateUniform { uniform | description = Just description } index
                , attrs = [ Forms.Placeholder "Description" ]
                }
            , span [ style "display" "inline-block", style "vertical-align" "middle" ]
                [ Buttons.delete <| TryToDeleteUniform index ]
            ]
    in
    tr []
        (columns |> List.map (\column -> td [] [ column ]))


newUniformRow : Uniform -> Html Msg
newUniformRow uniform =
    let
        columns =
            [ textInput Forms.string
                { value = uniform.name
                , onInput = InputNewName
                , attrs = [ Forms.Placeholder "Name" ]
                }
            , textareaInput
                { value = uniform.description |> Maybe.withDefault ""
                , onInput = InputNewDescription
                , attrs = [ Forms.Placeholder "Description" ]
                }
            , Buttons.button
                { content = "Suit up."
                , onClick = Just CreateNewUniform
                , attrs = [ Buttons.Color Buttons.IsPrimary ]
                }
            ]
    in
    tr []
        (columns |> List.map (\column -> td [] [ column ]))


deleteUniformModal : Model -> Html Msg
deleteUniformModal model =
    let
        uniformToDelete =
            model.tryingToDelete
                |> Maybe.andThen
                    (\index ->
                        model.uniforms
                            |> remoteToMaybe
                            |> Maybe.andThen (getAt index)
                    )
    in
    case uniformToDelete of
        Nothing ->
            text ""

        Just uniform ->
            deleteModal
                { title = "Delete uniform " ++ uniform.name ++ "?"
                , cancel = CancelDeleteUniform
                , confirm = ConfirmDeleteUniform
                , state = model.state
                , content =
                    div []
                        [ p [] [ text <| "Are you sure you want to delete the " ++ uniform.name ++ " uniform?" ]
                        , p [] [ i [] [ text "Note: all events that have this uniform will no longer have a uniform." ] ]
                        ]
                }
