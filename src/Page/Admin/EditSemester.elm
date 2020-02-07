module Page.Admin.EditSemester exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.Forms as Forms exposing (selectInput, textInput)
import Datetime exposing (hyphenDateFormatter, parseFormDateString)
import Error exposing (GreaseResult)
import Html exposing (Html, a, br, button, div, form, h2, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode exposing (string)
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra exposing (isNothing)
import Models.Info exposing (Semester, semesterDecoder)
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), getRequest, postRequest, resultToRemote)



---- MODEL ----


type alias Model =
    { common : Common
    , semesters : RemoteData (List Semester)
    , tab : Maybe EditSemesterTab
    , state : SubmissionState
    }


type EditSemesterTab
    = ChangingSemester String
    | CreatingSemester SemesterForm
    | EditingSemester String SemesterForm


type alias SemesterForm =
    { name : String
    , startDate : String
    , endDate : String
    , gigRequirement : Maybe Int
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , semesters = Loading
      , tab = Nothing
      , state = NotSentYet
      }
    , loadSemesters common
    )


emptySemesterForm : SemesterForm
emptySemesterForm =
    { name = ""
    , startDate = ""
    , endDate = ""
    , gigRequirement = Nothing
    }


formFromSemester : Common -> Semester -> SemesterForm
formFromSemester common semester =
    { name = semester.name
    , startDate = semester.startDate |> hyphenDateFormatter common.timeZone
    , endDate = semester.endDate |> hyphenDateFormatter common.timeZone
    , gigRequirement = Just semester.gigRequirement
    }



---- UPDATE ----


type Msg
    = OnLoadSemesters (GreaseResult (List Semester))
    | ChangeTab (Maybe EditSemesterTab)
    | SelectSemesterToChangeTo String
    | ChangeSemesterTo String
    | UpdateCreateSemesterForm SemesterForm
    | CreateSemester SemesterForm
    | UpdateEditSemesterForm SemesterForm
    | EditSemester String SemesterForm
    | OnChange (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadSemesters result ->
            ( { model | semesters = resultToRemote result }, Cmd.none )

        ChangeTab newTab ->
            ( { model
                | tab = newTab
                , state =
                    if isNothing newTab then
                        NotSentYet

                    else
                        model.state
              }
            , Cmd.none
            )

        SelectSemesterToChangeTo name ->
            case model.tab of
                Just (ChangingSemester _) ->
                    ( { model | tab = Just <| ChangingSemester name }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeSemesterTo name ->
            ( { model | state = Sending }, changeSemester model.common name )

        UpdateCreateSemesterForm semester ->
            case model.tab of
                Just (CreatingSemester _) ->
                    ( { model | tab = Just <| CreatingSemester semester }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CreateSemester semester ->
            ( { model | state = Sending }, createSemester model.common semester )

        UpdateEditSemesterForm semester ->
            case model.tab of
                Just (EditingSemester name _) ->
                    ( { model | tab = Just <| EditingSemester name semester }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditSemester name semester ->
            ( { model | state = Sending }, editSemester model.common name semester )

        OnChange (Ok _) ->
            ( model, Nav.reload )

        OnChange (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )



---- DATA ----


loadSemesters : Common -> Cmd Msg
loadSemesters common =
    getRequest common "/semesters" (Decode.list semesterDecoder)
        |> Task.attempt OnLoadSemesters


changeSemester : Common -> String -> Cmd Msg
changeSemester common semesterName =
    let
        url =
            "/semesters/" ++ semesterName ++ "/set_current"
    in
    postRequest common url (Encode.string "")
        |> Task.attempt OnChange


createSemester : Common -> SemesterForm -> Cmd Msg
createSemester common semesterForm =
    postRequest common "/semesters" (semesterForm |> serializeSemester common)
        |> Task.attempt OnChange


editSemester : Common -> String -> SemesterForm -> Cmd Msg
editSemester common name semesterForm =
    let
        url =
            "/semesters/" ++ name
    in
    postRequest common url (semesterForm |> serializeSemester common)
        |> Task.attempt OnChange


serializeSemester : Common -> SemesterForm -> Encode.Value
serializeSemester common semesterForm =
    let
        startDate =
            semesterForm.startDate
                |> parseFormDateString common
                |> Maybe.withDefault common.now

        endDate =
            semesterForm.endDate
                |> parseFormDateString common
                |> Maybe.withDefault common.now

        gigRequirement =
            semesterForm.gigRequirement
                |> Maybe.withDefault 5
    in
    Encode.object
        [ ( "name", Encode.string semesterForm.name )
        , ( "startDate", startDate |> posixToMillis |> Encode.int )
        , ( "endDate", endDate |> posixToMillis |> Encode.int )
        , ( "gigRequirement", Encode.int gigRequirement )
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Edit the Semester"
        , model.semesters |> Basics.remoteContent (editSemesterOptions model)
        ]


editSemesterOptions : Model -> List Semester -> Html Msg
editSemesterOptions model semesters =
    let
        semesterOption content semesterTab =
            Forms.control
                [ Buttons.button
                    { content = content
                    , onClick = Just <| (ChangeTab <| Just semesterTab)
                    , attrs = [ Buttons.Color Buttons.IsPrimary ]
                    }
                ]

        currentSemester =
            model.common.currentSemester
    in
    div []
        [ editSemesterPrelude
        , Buttons.group
            { alignment = Buttons.AlignCenter
            , connected = True
            , buttons =
                [ semesterOption "Switch semesters" (ChangingSemester currentSemester.name)
                , semesterOption "Edit this semester"
                    (EditingSemester currentSemester.name <|
                        formFromSemester model.common currentSemester
                    )
                , semesterOption "Birth a semester" (CreatingSemester <| emptySemesterForm)
                ]
            }
        , br [] []
        , currentSemesterTab model semesters
        ]


currentSemesterTab : Model -> List Semester -> Html Msg
currentSemesterTab model semesters =
    let
        semesterSidebar content =
            Basics.sidebar
                { close = ChangeTab Nothing
                , data = Loaded {}
                , render = \_ -> content
                }
    in
    case model.tab of
        Nothing ->
            text ""

        Just (ChangingSemester selectedName) ->
            changeSemesterModal model.state semesters selectedName

        Just (EditingSemester _ semester) ->
            semesterSidebar <| editSemesterSidebar model.state semester

        Just (CreatingSemester semester) ->
            semesterSidebar <| createSemesterSidebar model.state semester


editSemesterPrelude : Html Msg
editSemesterPrelude =
    Basics.box
        [ p []
            [ text "This is a form that will allow you to add and edit semesters in the database. "
            , text "It will also change the current semester."
            ]
        , br [] []
        , p []
            [ text "Changing the current semester will change the entire face of the website. "
            , text "Only stuff from the current semester is shown on the main website. "
            , text "In new semesters, every member's status is by default 'inactive' "
            , text "until they log in and confirm themself. "
            ]
        , br [] []
        , p []
            [ text "With great power comes great potential to screw everyone over. "
            , text "Use this feature wisely."
            ]
        , br [] []
        , p []
            [ text "Now, pick your poison:" ]
        ]


changeSemesterModal : SubmissionState -> List Semester -> String -> Html Msg
changeSemesterModal state semesters selectedName =
    div [ class "modal is-active" ]
        [ div [ class "modal-background", onClick (ChangeTab Nothing) ] []
        , div [ class "modal-content", style "text-align" "center" ]
            [ Basics.box [ changeSemesterModalContent state semesters selectedName ] ]
        ]


changeSemesterModalContent : SubmissionState -> List Semester -> String -> Html Msg
changeSemesterModalContent state semesters selectedName =
    Basics.column <|
        [ h2 [ class "subtitle is-2" ] [ text "Which semester do you want to switch to?" ]
        , p []
            [ text "This will change everything. You really only want to do this "
            , text "at the beginning of a new semester. If it's not a solstice, then don't."
            ]
        , br [] []
        , div
            [ class "field is-grouped is-grouped-centered" ]
            [ selectInput Forms.string
                { values =
                    semesters
                        |> List.map .name
                        |> List.updateIf ((==) selectedName) ((++) " (current)")
                , selected = selectedName
                , onInput = SelectSemesterToChangeTo
                , attrs = []
                }
            ]
        , br [] []
        , Buttons.button
            { content = "The ol' Glub Hub switcharoo"
            , onClick = Just <| ChangeSemesterTo selectedName
            , attrs =
                [ Buttons.CustomElement a
                , Buttons.CustomAttrs [ class "is-pulled-left" ]
                , Buttons.Color Buttons.IsPrimary
                , Buttons.IsLoading (state == Sending)
                ]
            }
        , Buttons.button
            { content = "ABORT! ABORT!"
            , onClick = Just <| ChangeTab Nothing
            , attrs =
                [ Buttons.CustomElement a
                , Buttons.CustomAttrs [ class "is-pulled-right" ]
                ]
            }
        , br [] []
        ]
            ++ (case state of
                    ErrorSending error ->
                        [ br [] [], Basics.errorBox error ]

                    _ ->
                        []
               )


createSemesterSidebar : SubmissionState -> SemesterForm -> Html Msg
createSemesterSidebar state semester =
    div [ class "column", style "text-align" "center" ]
        [ h2 [ class "subtitle is-2" ]
            [ text "Time marches on" ]
        , p []
            [ text "Another day, another dollar. And also another semester. "
            , text "Make a new semester baby now, and switch over to it whenever "
            , text "you want to later, but before it turns 18."
            ]
        , br [] []
        , Basics.form (CreateSemester semester)
            [ Basics.narrowColumn
                [ textInput Forms.string
                    { value = semester.name
                    , onInput = \name -> UpdateCreateSemesterForm { semester | name = name }
                    , attrs =
                        [ Forms.Title "Semester Name"
                        , Forms.Placeholder "Fall 20XX"
                        , Forms.RequiredField True
                        ]
                    }
                , textInput Forms.date
                    { value = semester.startDate
                    , onInput = \startDate -> UpdateCreateSemesterForm { semester | startDate = startDate }
                    , attrs =
                        [ Forms.Title "The first day of the rest of your life"
                        , Forms.RequiredField True
                        ]
                    }
                , textInput Forms.date
                    { value = semester.endDate
                    , onInput = \endDate -> UpdateCreateSemesterForm { semester | endDate = endDate }
                    , attrs =
                        [ Forms.Title "The last day of the rest of your life"
                        , Forms.RequiredField True
                        ]
                    }
                , textInput Forms.int
                    { value = semester.gigRequirement
                    , onInput = \gigRequirement -> UpdateCreateSemesterForm { semester | gigRequirement = gigRequirement }
                    , attrs =
                        [ Forms.Title "Number of required volunteer gigs"
                        , Forms.RequiredField True
                        , Forms.Placeholder "5"
                        ]
                    }
                , br [] []
                , Buttons.submit
                    { content = "Break your water"
                    , attrs =
                        [ Buttons.Color Buttons.IsPrimary
                        , Buttons.IsLoading (state == Sending)
                        ]
                    }
                , br [] []
                , case state of
                    ErrorSending error ->
                        Basics.errorBox error

                    _ ->
                        text ""
                ]
            ]
        ]


editSemesterSidebar : SubmissionState -> SemesterForm -> Html Msg
editSemesterSidebar state semester =
    div [ class "column", style "text-align" "center" ]
        [ h2 [ class "subtitle is-2" ]
            [ text "Do it in pencil, not pen" ]
        , p []
            [ text "Select the semester you want to make changes to, and then "
            , text "make the changes. You cannot edit the past, sorry."
            ]
        , br [] []
        , form [ onSubmit <| CreateSemester semester ]
            [ Basics.narrowColumn
                [ textInput Forms.string
                    { value = semester.name
                    , onInput = \name -> UpdateEditSemesterForm { semester | name = name }
                    , attrs =
                        [ Forms.Title "Semester Name"
                        , Forms.Placeholder "Fall 20XX"
                        , Forms.RequiredField True
                        ]
                    }
                , textInput Forms.date
                    { value = semester.startDate
                    , onInput = \startDate -> UpdateEditSemesterForm { semester | startDate = startDate }
                    , attrs =
                        [ Forms.Title "The first day of the rest of your life"
                        , Forms.RequiredField True
                        ]
                    }
                , textInput Forms.date
                    { value = semester.endDate
                    , onInput = \endDate -> UpdateEditSemesterForm { semester | endDate = endDate }
                    , attrs =
                        [ Forms.Title "The last day of the rest of your life"
                        , Forms.RequiredField True
                        ]
                    }
                , textInput Forms.int
                    { value = semester.gigRequirement
                    , onInput = \gigRequirement -> UpdateEditSemesterForm { semester | gigRequirement = gigRequirement }
                    , attrs =
                        [ Forms.Title "Number of required volunteer gigs"
                        , Forms.Placeholder "5"
                        , Forms.RequiredField True
                        ]
                    }
                , br [] []
                , Buttons.submit
                    { content = "Do this please"
                    , attrs =
                        [ Buttons.Color Buttons.IsPrimary
                        , Buttons.IsLoading (state == Sending)
                        ]
                    }
                , br [] []
                , case state of
                    ErrorSending error ->
                        Basics.errorBox error

                    _ ->
                        text ""
                ]
            ]
        ]
