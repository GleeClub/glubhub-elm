module Components.ConfirmAccount exposing (Model, Msg, confirmAccountHeader, init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.Forms as Forms
import Error exposing (GreaseResult)
import Html exposing (Html, div, form, section, span, text)
import Html.Attributes exposing (class, style)
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Models.Info exposing (Enrollment(..), enrollmentToString)
import Request
import Task
import Utils exposing (Common, SubmissionState(..))


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
            [ Buttons.delete data.ignoreConfirm
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
                    [ Buttons.button
                        { content = "Confirm"
                        , onClick = Just data.confirmAccount
                        , attrs =
                            [ Buttons.Color Buttons.IsInfo
                            , Buttons.IsInverted
                            , Buttons.IsOutlined
                            , Buttons.CustomAttrs [ style "margin" "0 2em" ]
                            ]
                        }
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
    Request.post common url body
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

        onCampusButton onCampus title =
            Buttons.button
                { content = title
                , onClick = Just <| UpdateForm { semesterForm | onCampus = onCampus }
                , attrs =
                    [ Just (Buttons.CustomElement span)
                    , Just (Buttons.Color Buttons.IsPrimary)
                        |> Maybe.filter (\_ -> model.form.onCampus == onCampus)
                    ]
                        |> List.filterMap identity
                }

        enrollmentButton enrollment title =
            Buttons.button
                { content = title
                , onClick = Just <| UpdateForm { semesterForm | enrollment = enrollment }
                , attrs =
                    [ Just (Buttons.CustomElement span)
                    , Just (Buttons.Color Buttons.IsPrimary)
                        |> Maybe.filter (\_ -> model.form.enrollment == enrollment)
                    ]
                        |> List.filterMap identity
                }

        sectionFormType =
            { toString = Maybe.withDefault "No Section"
            , fromString = \s -> model.common.info.sections |> List.find ((==) s)
            , textType = Forms.Text
            }
    in
    Basics.section
        [ Basics.form SubmitForm
            [ Basics.title4 "Confirm Your Account"
            , Forms.inputWrapper [ Forms.Horizontal, Forms.Title "Location" ]
                [ Forms.textInput Forms.string
                    { value = model.form.location
                    , onInput = \location -> UpdateForm { semesterForm | location = location }
                    , attrs =
                        [ Forms.Placeholder "Glenn"
                        , Forms.RequiredField True
                        ]
                    }
                , Forms.control
                    [ Buttons.group
                        { alignment = Buttons.AlignLeft
                        , connected = True
                        , buttons =
                            [ onCampusButton True "On-campus"
                            , onCampusButton False "Off-campus"
                            ]
                        }
                    ]
                ]
            , Forms.inputWrapper [ Forms.Horizontal, Forms.Title "Enrollment" ]
                [ Forms.selectInput sectionFormType
                    { values = model.common.info.sections |> List.map Just
                    , selected = model.form.section
                    , onInput = \s -> UpdateForm { semesterForm | section = s }
                    , attrs = []
                    }
                , Forms.control
                    [ Buttons.group
                        { alignment = Buttons.AlignLeft
                        , connected = True
                        , buttons =
                            [ enrollmentButton Class "Class"
                            , enrollmentButton Club "Club"
                            ]
                        }
                    ]
                ]
            , Buttons.group
                { alignment = Buttons.AlignRight
                , connected = False
                , buttons =
                    [ Buttons.submit
                        { content = "Save"
                        , attrs = [ Buttons.Color Buttons.IsPrimary ]
                        }
                    ]
                }
            ]
        ]
