module Components.ConfirmAccount exposing (Model, Msg, confirmAccountHeader, init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, a, button, div, form, h4, input, label, option, p, section, select, span, text)
import Html.Attributes exposing (class, name, placeholder, selected, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Encode as Encode
import Models.Info exposing (Enrollment(..), enrollmentToString)
import Task
import Utils exposing (Common, SubmissionState(..), postRequest)


type alias ConfirmAccountHeader msg =
    { ignoreConfirm : msg
    , confirmAccount : msg
    }


confirmAccountHeader : ConfirmAccountHeader msg -> Html msg
confirmAccountHeader data =
    section
        [ style "margin" "2em"
        , style "margin-bottom" "-1em"
        , style "padding-top" "40px"
        ]
        [ div [ class "notification is-info" ]
            [ button [ class "delete", onClick data.ignoreConfirm ] []
            , div
                [ style "width" "100%"
                , style "display" "flex"
                , style "align-items" "center"
                ]
                [ div []
                    [ text "Welcome! Feel free to browse the site, but "
                    , text "if you're going to be active in Glee Club this "
                    , text "semester, please confirm your account so we "
                    , text "can get you into the system."
                    ]
                , div []
                    [ a
                        [ class "button is-info is-inverted is-outlined"
                        , style "margin" "0 2em"
                        , onClick data.confirmAccount
                        ]
                        [ text "Confirm" ]
                    ]
                ]
            ]
        ]


type alias Model =
    { common : Common
    , form : SemesterForm
    , state : SubmissionState
    }


type alias SemesterForm =
    { location : String
    , onCampus : Bool
    , enrollment : Enrollment
    , section : Maybe String
    }


init : Common -> Model
init common =
    let
        semesterForm =
            { location =
                common.user
                    |> Maybe.map .location
                    |> Maybe.withDefault ""
            , onCampus =
                common.user
                    |> Maybe.andThen .onCampus
                    |> Maybe.withDefault True
            , enrollment = Class
            , section = common.info.sections |> List.head
            }
    in
    { common = common
    , form = semesterForm
    , state = NotSentYet
    }


type Msg
    = UpdateForm SemesterForm
    | SubmitForm
    | OnSubmitForm (GreaseResult ())


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        UpdateForm semesterForm ->
            ( { model | form = semesterForm }, Cmd.none )

        SubmitForm ->
            ( { model | state = Sending }, submitForm model.common model.form )

        OnSubmitForm (Ok _) ->
            ( model, Nav.reload )

        OnSubmitForm (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )


submitForm : Common -> SemesterForm -> Cmd Msg
submitForm common semesterForm =
    let
        body =
            serializeSemesterForm semesterForm

        url =
            "/members/confirm"
    in
    postRequest common url body
        |> Task.attempt OnSubmitForm


serializeSemesterForm : SemesterForm -> Encode.Value
serializeSemesterForm semesterForm =
    Encode.object
        [ ( "location", Encode.string semesterForm.location )
        , ( "onCampus", Encode.bool semesterForm.onCampus )
        , ( "conflicts", Encode.string "" )
        , ( "dietaryRestrictions", Encode.string "" )
        , ( "enrollment", Encode.string (enrollmentToString semesterForm.enrollment) )
        , ( "section", Encode.string (semesterForm.section |> Maybe.withDefault "") )
        ]


view : Model -> Html Msg
view model =
    let
        semesterForm =
            model.form

        onCampusButton val title =
            span
                [ class <|
                    "button"
                        ++ (if model.form.onCampus == val then
                                " is-primary"

                            else
                                ""
                           )
                , onClick <| UpdateForm { semesterForm | onCampus = val }
                ]
                [ text title ]

        enrollmentButton val title =
            span
                [ class <|
                    "button"
                        ++ (if model.form.enrollment == val then
                                " is-primary"

                            else
                                ""
                           )
                , onClick <| UpdateForm { semesterForm | enrollment = val }
                ]
                [ text title ]
    in
    section [ class "section" ]
        [ form [ onSubmit SubmitForm ]
            [ h4 [ class "title is-4" ] [ text "Confirm Your Account" ]
            , div [ class "field is-horizontal" ]
                [ div [ class "field-label is-normal" ]
                    [ label [ class "label" ] [ text "Location" ] ]
                , div [ class "field-body" ]
                    [ div [ class "field is-grouped" ]
                        [ p [ class "control is-expanded" ]
                            [ input
                                [ class "input"
                                , type_ "text"
                                , name "location"
                                , value model.form.location
                                , placeholder "Glenn"
                                , onInput (\location -> UpdateForm { semesterForm | location = location })
                                ]
                                []
                            ]
                        , p [ class "control" ]
                            [ div [ class "buttons has-addons" ]
                                [ onCampusButton True "On-campus"
                                , onCampusButton False "Off-campus"
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "field is-horizontal" ]
                [ div [ class "field-label is-normal" ]
                    [ label [ class "label" ] [ text "Enrollment" ] ]
                , div [ class "field-body" ]
                    [ div [ class "field is-grouped" ]
                        [ div [ class "control" ]
                            [ div [ class "select" ]
                                [ select [ onInput (\s -> UpdateForm { semesterForm | section = Just s }) ]
                                    (model.common.info.sections
                                        |> List.map
                                            (\s ->
                                                option
                                                    [ value s
                                                    , selected
                                                        (model.form.section
                                                            |> Maybe.map ((==) s)
                                                            |> Maybe.withDefault False
                                                        )
                                                    ]
                                                    [ text s ]
                                            )
                                    )
                                ]
                            ]
                        , div [ class "control" ]
                            [ div [ class "buttons has-addons" ]
                                [ enrollmentButton Class "Class"
                                , enrollmentButton Club "Club"
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "buttons is-right" ]
                [ button [ type_ "submit", class "button is-primary" ] [ text "Save" ] ]
            ]
        ]
