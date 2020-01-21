module Page.Minutes exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.DeleteModal exposing (deleteModal)
import Components.SelectableList exposing (selectableListFull)
import Error exposing (GreaseResult)
import Html exposing (Html, a, div, input, li, p, section, td, text, ul)
import Html.Attributes exposing (class, id, placeholder, required, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra exposing (filter, isJust)
import Models.Document exposing (MeetingMinutes, meetingMinutesDecoder)
import Models.Event exposing (Member)
import Permissions exposing (editMinutes, viewCompleteMinutes)
import Route exposing (MinutesRoute, MinutesTab(..))
import Task
import Utils
    exposing
        ( Common
        , RemoteData(..)
        , SubmissionState(..)
        , deleteRequest
        , deployEditor
        , getRequest
        , isLoadingClass
        , isPrimaryClass
        , mapLoaded
        , optionalSingleton
        , permittedTo
        , postRequest
        , postRequestFull
        , rawHtml
        , remoteToMaybe
        , resultToRemote
        )



---- MODEL ----


type alias Model =
    { common : Common
    , minutes : RemoteData (List MeetingMinutes)
    , selected : RemoteData ( MeetingMinutes, FullMinutesTab )
    , state : SubmissionState
    , expanded : Bool
    }


type FullMinutesTab
    = FullPublicMinutes
    | FullPrivateMinutes
    | FullEditMinutes EditMinutesContext


type alias EditMinutesContext =
    { isPublic : Bool
    , minutes : MeetingMinutes
    , state : SubmissionState
    , deleteState : Maybe SubmissionState
    }


init : Common -> MinutesRoute -> ( Model, Cmd Msg )
init common route =
    let
        ( selectedMinutes, maybeLoadSingleMinutes ) =
            case route.id of
                Just selectedId ->
                    ( Loading, [ loadSingleMinutes common selectedId (route.tab |> Maybe.withDefault PublicMinutes) ] )

                Nothing ->
                    ( NotAsked, [] )

        commands =
            loadAllMinutes common :: maybeLoadSingleMinutes
    in
    ( { common = common
      , minutes = Loading
      , selected = selectedMinutes
      , state = NotSentYet
      , expanded = False
      }
    , Cmd.batch commands
    )


minutesEditorId : String
minutesEditorId =
    "minutesEditor"



---- UPDATE ----


type Msg
    = OnLoadAllMinutes (GreaseResult (List MeetingMinutes))
    | OnLoadSingleMinutes MinutesTab (GreaseResult MeetingMinutes)
    | SelectMinutes Int
    | SelectTab MinutesTab
    | CreateNewMinutes
    | OnCreateNewMinutes (GreaseResult Int)
    | UpdateEditingMinutes MeetingMinutes
    | SwitchEditingPublicOrPrivate Bool
    | SaveEditingMinutes
    | OnSaveEditingMinutes (GreaseResult MeetingMinutes)
    | ToggleShowAllMinutes
    | TryToDeleteMinutes
    | CancelDeletingMinutes
    | SendDeleteMinutes
    | OnDeleteMinutes (GreaseResult Int)


newMinutesTitle : String
newMinutesTitle =
    "New Meeting"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadAllMinutes result ->
            ( { model | minutes = result |> resultToRemote }, Cmd.none )

        OnLoadSingleMinutes minutesTab result ->
            let
                newModel =
                    { model | selected = result |> Result.map (\s -> ( s, FullPublicMinutes )) |> resultToRemote }
            in
            selectTab newModel minutesTab

        SelectMinutes selected ->
            let
                currentId =
                    case model.selected of
                        Loaded ( minutes, _ ) ->
                            Just minutes.id

                        _ ->
                            Nothing
            in
            if currentId |> filter (\id -> id == selected) |> isJust then
                ( model, Cmd.none )

            else
                ( { model | selected = Loading }
                , Cmd.batch
                    [ loadSingleMinutes model.common selected PublicMinutes
                    , Route.replaceUrl model.common.key <|
                        Route.Minutes { id = Just selected, tab = Just PublicMinutes }
                    ]
                )

        SelectTab tab ->
            selectTab model tab

        CreateNewMinutes ->
            ( { model | state = Sending }, createNewMinutes model.common )

        OnCreateNewMinutes (Ok newId) ->
            let
                newMinutes =
                    { id = newId
                    , name = newMinutesTitle
                    , public = Nothing
                    , private = Nothing
                    , date = model.common.now
                    }

                newModel =
                    { model
                        | minutes = model.minutes |> mapLoaded ((::) newMinutes)
                        , selected = Loaded ( newMinutes, FullPrivateMinutes )
                        , state = NotSentYet
                    }
            in
            selectTab newModel PublicMinutes

        OnCreateNewMinutes (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )

        UpdateEditingMinutes updatedMinutes ->
            ( { model
                | selected =
                    model.selected
                        |> mapLoaded
                            (\( minutes, tab ) ->
                                case tab of
                                    FullEditMinutes context ->
                                        ( minutes, FullEditMinutes { context | minutes = updatedMinutes } )

                                    other ->
                                        ( minutes, other )
                            )
              }
            , Cmd.none
            )

        SwitchEditingPublicOrPrivate isPublic ->
            case model.selected of
                Loaded ( minutes, FullEditMinutes context ) ->
                    ( { model
                        | selected =
                            Loaded ( minutes, FullEditMinutes { context | isPublic = isPublic } )
                      }
                    , deployEditor
                        { elementId = minutesEditorId
                        , content =
                            (if isPublic then
                                minutes.public

                             else
                                minutes.private
                            )
                                |> Maybe.withDefault ""
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        SaveEditingMinutes ->
            case model.selected of
                Loaded ( minutes, FullEditMinutes context ) ->
                    ( { model
                        | state = Sending
                        , selected =
                            Loaded ( minutes, FullEditMinutes { context | state = Sending } )
                      }
                    , updateMinutes model.common context.minutes
                    )

                _ ->
                    ( model, Cmd.none )

        OnSaveEditingMinutes (Ok updatedMinutes) ->
            let
                mapper minutes =
                    if minutes.id == updatedMinutes.id then
                        updatedMinutes

                    else
                        minutes
            in
            ( { model
                | state = NotSentYet
                , selected = Loaded ( updatedMinutes, FullPublicMinutes )
                , minutes = model.minutes |> mapLoaded (List.map mapper)
              }
            , Cmd.none
            )

        OnSaveEditingMinutes (Err error) ->
            ( { model
                | selected =
                    model.selected
                        |> mapLoaded
                            (\( minutes, tab ) ->
                                case tab of
                                    FullEditMinutes context ->
                                        ( minutes, FullEditMinutes { context | state = ErrorSending error } )

                                    other ->
                                        ( minutes, other )
                            )
              }
            , Cmd.none
            )

        ToggleShowAllMinutes ->
            ( { model | expanded = not model.expanded }, Cmd.none )

        TryToDeleteMinutes ->
            case model.selected of
                Loaded ( minutes, FullEditMinutes context ) ->
                    ( { model
                        | selected =
                            Loaded
                                ( minutes
                                , FullEditMinutes { context | deleteState = Just NotSentYet }
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CancelDeletingMinutes ->
            case model.selected of
                Loaded ( minutes, FullEditMinutes context ) ->
                    ( { model
                        | selected =
                            Loaded
                                ( minutes
                                , FullEditMinutes { context | deleteState = Nothing }
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SendDeleteMinutes ->
            case model.selected of
                Loaded ( minutes, FullEditMinutes context ) ->
                    ( { model
                        | selected =
                            Loaded
                                ( minutes
                                , FullEditMinutes { context | deleteState = Just Sending }
                                )
                      }
                    , deleteMinutes model.common minutes.id
                    )

                _ ->
                    ( model, Cmd.none )

        OnDeleteMinutes (Ok minutesId) ->
            ( { model
                | selected = NotAsked
                , minutes =
                    model.minutes
                        |> mapLoaded (List.filter (\m -> m.id /= minutesId))
              }
            , Route.replaceUrl model.common.key <|
                Route.Minutes { id = Nothing, tab = Nothing }
            )

        OnDeleteMinutes (Err error) ->
            case model.selected of
                Loaded ( minutes, FullEditMinutes context ) ->
                    ( { model
                        | state = Sending
                        , selected =
                            Loaded
                                ( minutes
                                , FullEditMinutes { context | deleteState = Just <| ErrorSending error }
                                )
                      }
                    , updateMinutes model.common context.minutes
                    )

                _ ->
                    ( model, Cmd.none )


selectTab : Model -> MinutesTab -> ( Model, Cmd Msg )
selectTab model tab =
    case model.selected of
        Loaded ( minutes, currentTab ) ->
            case ( tab, currentTab ) of
                ( PublicMinutes, FullPublicMinutes ) ->
                    ( model, Cmd.none )

                ( PrivateMinutes, FullPrivateMinutes ) ->
                    ( model, Cmd.none )

                ( EditMinutes, FullEditMinutes _ ) ->
                    ( model, Cmd.none )

                ( PublicMinutes, _ ) ->
                    let
                        newModel =
                            { model | selected = Loaded ( minutes, FullPublicMinutes ) }
                    in
                    ( newModel, updateUrl newModel )

                ( PrivateMinutes, _ ) ->
                    let
                        newModel =
                            { model | selected = Loaded ( minutes, FullPrivateMinutes ) }
                    in
                    ( newModel, updateUrl newModel )

                ( EditMinutes, _ ) ->
                    let
                        newModel =
                            { model
                                | selected =
                                    Loaded
                                        ( minutes
                                        , FullEditMinutes
                                            { isPublic = False
                                            , minutes = minutes
                                            , state = NotSentYet
                                            , deleteState = Nothing
                                            }
                                        )
                            }
                    in
                    ( newModel
                    , Cmd.batch
                        [ deployEditor
                            { elementId = minutesEditorId
                            , content = minutes.private |> Maybe.withDefault ""
                            }
                        , updateUrl newModel
                        ]
                    )

        _ ->
            ( model, Cmd.none )



---- DATA ----


updateUrl : Model -> Cmd Msg
updateUrl model =
    Route.replaceUrl model.common.key <|
        Route.Minutes
            { id =
                model.selected
                    |> remoteToMaybe
                    |> Maybe.map (Tuple.first >> .id)
            , tab =
                model.selected
                    |> remoteToMaybe
                    |> Maybe.map (\( _, tab ) -> simplifyFullMinutesTab tab)
            }


loadAllMinutes : Common -> Cmd Msg
loadAllMinutes common =
    getRequest common "/meeting_minutes" (Decode.list meetingMinutesDecoder)
        |> Task.attempt OnLoadAllMinutes


loadSingleMinutes : Common -> Int -> MinutesTab -> Cmd Msg
loadSingleMinutes common minutesId tab =
    let
        url =
            "/meeting_minutes/" ++ String.fromInt minutesId
    in
    getRequest common url meetingMinutesDecoder
        |> Task.attempt (OnLoadSingleMinutes tab)


createNewMinutes : Common -> Cmd Msg
createNewMinutes common =
    let
        value =
            Encode.object [ ( "name", Encode.string newMinutesTitle ) ]

        idDecoder =
            Decode.field "id" Decode.int
    in
    postRequestFull common "/meeting_minutes" value idDecoder
        |> Task.attempt OnCreateNewMinutes


updateMinutes : Common -> MeetingMinutes -> Cmd Msg
updateMinutes common minutes =
    let
        url =
            "/meeting_minutes/" ++ String.fromInt minutes.id

        body =
            serializeMinutes minutes
    in
    postRequest common url body
        |> Task.map (\_ -> minutes)
        |> Task.attempt OnSaveEditingMinutes


serializeMinutes : MeetingMinutes -> Encode.Value
serializeMinutes minutes =
    Encode.object
        [ ( "name", Encode.string minutes.name )
        , ( "public"
          , Encode.string (minutes.public |> Maybe.withDefault "")
          )
        , ( "private"
          , Encode.string (minutes.private |> Maybe.withDefault "")
          )
        ]


deleteMinutes : Common -> Int -> Cmd Msg
deleteMinutes common minutesId =
    let
        url =
            "/meeting_minutes/" ++ String.fromInt minutesId
    in
    deleteRequest common url
        |> Task.map (\_ -> minutesId)
        |> Task.attempt OnDeleteMinutes


simplifyFullMinutesTab : FullMinutesTab -> MinutesTab
simplifyFullMinutesTab fullTab =
    case fullTab of
        FullPublicMinutes ->
            PublicMinutes

        FullPrivateMinutes ->
            PrivateMinutes

        FullEditMinutes _ ->
            EditMinutes



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        isSelected minutes =
            model.selected
                |> remoteToMaybe
                |> Maybe.map (\( s, _ ) -> s.id == minutes.id)
                |> Maybe.withDefault False

        textButton isPrimary clickHandler buttonText =
            div
                [ class "field is-grouped is-grouped-centered"
                , style "padding-bottom" "5px"
                ]
                [ p [ class "control" ]
                    [ a
                        [ class <| "button" ++ Utils.isPrimaryClass isPrimary
                        , onClick clickHandler
                        ]
                        [ text buttonText ]
                    ]
                ]

        minutesList =
            selectableListFull
                { listItems =
                    if not model.expanded then
                        model.minutes |> mapLoaded (List.take 10)

                    else
                        model.minutes
                , render = \minutes -> [ td [] [ text minutes.name ] ]
                , onSelect = \minutes -> SelectMinutes minutes.id
                , messageIfEmpty = "No minutes"
                , isSelected = isSelected
                , contentAtTop =
                    Basics.renderIfHasPermission model.common editMinutes <|
                        textButton True CreateNewMinutes "+ Add New Minutes"
                , contentAtBottom =
                    if
                        model.minutes
                            |> remoteToMaybe
                            |> Maybe.map (\minutes -> List.length minutes > 10)
                            |> Maybe.withDefault False
                    then
                        textButton False ToggleShowAllMinutes <|
                            if model.expanded then
                                "Hide old minutes..."

                            else
                                "Show old minutes..."

                    else
                        text ""
                }
    in
    div [ id "minutes" ]
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ minutesList
                    , div [ class "column" ]
                        [ viewSelectedMinutes model ]
                    ]
                ]
            ]
        ]


viewSelectedMinutes : Model -> Html Msg
viewSelectedMinutes model =
    let
        notSelected =
            p [] [ text "Select minutes" ]

        render ( minutes, tab ) =
            div [] <|
                selectedMinutesTabBar model.common.user tab
                    :: selectedMinutesTab minutes tab model.common.user
    in
    Basics.box [ model.selected |> Basics.remoteContentFull notSelected render ]


selectedMinutesTabBar : Maybe Member -> FullMinutesTab -> Html Msg
selectedMinutesTabBar user selectedTab =
    let
        canViewCompleteMinutes =
            user |> Maybe.map (permittedTo viewCompleteMinutes) |> Maybe.withDefault False

        canEditMinutes =
            user |> Maybe.map (permittedTo editMinutes) |> Maybe.withDefault False

        tabs =
            List.concat <|
                [ singleTab PublicMinutes selectedTab
                    |> List.singleton
                , singleTab PrivateMinutes selectedTab
                    |> optionalSingleton canViewCompleteMinutes
                , singleTab EditMinutes selectedTab
                    |> optionalSingleton canEditMinutes
                ]
    in
    if canViewCompleteMinutes || canEditMinutes then
        div [ class "tabs" ] [ ul [] tabs ]

    else
        div [ style "display" "none" ] []


tabName : MinutesTab -> String
tabName tab =
    case tab of
        PublicMinutes ->
            "Redacted"

        PrivateMinutes ->
            "Complete"

        EditMinutes ->
            "Edit"


tabIsSelected : FullMinutesTab -> MinutesTab -> Bool
tabIsSelected currentTab tab =
    case ( currentTab, tab ) of
        ( FullPublicMinutes, PublicMinutes ) ->
            True

        ( FullPrivateMinutes, PrivateMinutes ) ->
            True

        ( FullEditMinutes _, EditMinutes ) ->
            True

        ( _, _ ) ->
            False


singleTab : MinutesTab -> FullMinutesTab -> Html Msg
singleTab tab selectedTab =
    li
        [ class <|
            if tabIsSelected selectedTab tab then
                "is-active"

            else
                ""
        ]
        [ a
            [ onClick <| SelectTab tab
            ]
            [ text <| tabName tab ]
        ]


selectedMinutesTab : MeetingMinutes -> FullMinutesTab -> Maybe Member -> List (Html Msg)
selectedMinutesTab minutes tab user =
    case tab of
        FullPublicMinutes ->
            minutes.public |> Maybe.withDefault "" |> rawHtml

        FullPrivateMinutes ->
            if user |> Maybe.map (permittedTo viewCompleteMinutes) |> Maybe.withDefault False then
                minutes.private |> Maybe.withDefault "" |> rawHtml

            else
                [ text "Slow down, cowboy! Who said you could see these here documents?" ]

        FullEditMinutes context ->
            if user |> Maybe.map (permittedTo editMinutes) |> Maybe.withDefault False then
                [ p [] [ editHeader context ]
                , p [] [ minutesEditor context ]
                , case context.state of
                    ErrorSending error ->
                        p [] [ Basics.errorBox error ]

                    _ ->
                        text ""
                , context.deleteState
                    |> Maybe.map
                        (\state ->
                            deleteModal
                                { title = "Delete this meeting?"
                                , content =
                                    div []
                                        [ text "Are you sure you want to delete these meeting"
                                        , text " minutes? You can't undo that."
                                        ]
                                , cancel = CancelDeletingMinutes
                                , confirm = SendDeleteMinutes
                                , state = state
                                }
                        )
                    |> Maybe.withDefault (text "")
                ]

            else
                [ text "Slow down, cowboy! Who said you could edit these here documents?" ]


minutesEditor : EditMinutesContext -> Html Msg
minutesEditor { isPublic, minutes } =
    div
        [ id minutesEditorId
        , class "pell"
        , on "oninput"
            (Decode.field "data" Decode.string
                |> Decode.map
                    (\content ->
                        UpdateEditingMinutes <|
                            if isPublic then
                                { minutes | public = Just content }

                            else
                                { minutes | private = Just content }
                    )
            )
        ]
        []


editHeader : EditMinutesContext -> Html Msg
editHeader context =
    let
        minutes =
            context.minutes

        titleField =
            div [ class "field has-addons is-expanded" ]
                [ p [ class "control" ]
                    [ a [ class "button is-static" ] [ text "Title" ] ]
                , p [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , value context.minutes.name
                        , placeholder "Secret Evil Meeting of Doom"
                        , required True
                        , onInput
                            (\name -> UpdateEditingMinutes { minutes | name = name })
                        ]
                        []
                    ]
                ]

        publicOrPrivate =
            div [ class "field has-addons" ]
                [ p [ class "control" ]
                    [ a
                        [ class <| "button" ++ isPrimaryClass context.isPublic
                        , onClick <| SwitchEditingPublicOrPrivate True
                        ]
                        [ text "Public" ]
                    ]
                , div [ class "control" ]
                    [ a
                        [ class <| "button" ++ isPrimaryClass (not context.isPublic)
                        , onClick <| SwitchEditingPublicOrPrivate False
                        ]
                        [ text "Private" ]
                    ]
                ]

        saveButton =
            div [ class "field is-grouped is-grouped-right" ]
                [ p [ class "control" ]
                    [ a
                        [ class <| "button is-primary" ++ isLoadingClass (context.state == Sending)
                        , onClick SaveEditingMinutes
                        ]
                        [ text "Save" ]
                    ]
                , p [ class "control" ]
                    [ a
                        [ class <| "button is-danger" ++ isLoadingClass (context.deleteState == Just Sending)
                        , onClick TryToDeleteMinutes
                        ]
                        [ text "Delete" ]
                    ]
                ]
    in
    div
        [ class "field is-horizontal is-expanded"
        , style "margin-bottom" "10px"
        ]
        [ div [ class "field-body" ]
            [ publicOrPrivate
            , titleField
            , saveButton
            ]
        ]
