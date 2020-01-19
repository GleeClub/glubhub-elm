module Page.Admin.Dues exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Forms exposing (checkboxInput, fieldWrapper, numberInputWithPrefix, selectInput, textInput)
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, h1, h3, input, label, li, p, span, table, td, text, tr, ul)
import Html.Attributes exposing (attribute, class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Admin exposing (Fee, feeDecoder)
import Models.Event exposing (Member)
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), fullName, getRequest, isLoadingClass, mapLoaded, postRequest, resultToRemote, resultToSubmissionState)



---- MODEL ----


type alias Model =
    { common : Common
    , fees : RemoteData (List Fee)
    , state : SubmissionState
    , tab : Maybe DuesTab
    }


type DuesTab
    = AssignDues SubmissionState
    | AssignLateDues SubmissionState
    | BatchTransactions TransactionBatch SubmissionState


type alias TransactionBatch =
    { members : List String
    , amount : Maybe Int
    , type_ : String
    , description : String
    }


emptyTransactionBatch : TransactionBatch
emptyTransactionBatch =
    { members = []
    , amount = Nothing
    , type_ = "Expense"
    , description = ""
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , fees = Loading
      , state = NotSentYet
      , tab = Nothing
      }
    , loadFees common
    )



---- UPDATE ----


type Msg
    = OnLoadFees (GreaseResult (List Fee))
    | UpdateFeeAmount Fee String
    | OnUpdateFee (GreaseResult ())
    | ChangeTab (Maybe DuesTab)
    | ChargeDues
    | OnChargeDues (GreaseResult ())
    | ChargeLateDues
    | OnChargeLateDues (GreaseResult ())
    | UpdateTransactionBatch TransactionBatch
    | SendTransactionBatch
    | OnSendTransactionBatch (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadFees feesResult ->
            ( { model | fees = resultToRemote feesResult }, Cmd.none )

        OnUpdateFee result ->
            ( { model | state = resultToSubmissionState result }, Cmd.none )

        ChangeTab tab ->
            ( { model | tab = tab }, Cmd.none )

        UpdateTransactionBatch batch ->
            case model.tab of
                Just (BatchTransactions _ state) ->
                    ( { model | tab = Just <| BatchTransactions batch state }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateFeeAmount fee stringAmount ->
            case stringAmount |> String.toInt of
                Nothing ->
                    ( model, Cmd.none )

                Just amount ->
                    let
                        updatedFee =
                            { fee | amount = amount }

                        feeMapper f =
                            if f.name == fee.name then
                                updatedFee

                            else
                                f
                    in
                    ( { model
                        | fees = model.fees |> mapLoaded (List.map feeMapper)
                        , state = Sending
                      }
                    , updateFeeAmount model.common updatedFee
                    )

        ChargeDues ->
            ( { model | tab = Just <| AssignDues Sending }
            , chargeDues model.common
            )

        OnChargeDues (Ok _) ->
            ( { model | tab = Nothing }, Cmd.none )

        OnChargeDues (Err error) ->
            ( { model | tab = Just <| AssignDues <| ErrorSending error }, Cmd.none )

        ChargeLateDues ->
            ( { model | tab = Just <| AssignLateDues Sending }
            , chargeLateDues model.common
            )

        OnChargeLateDues (Ok _) ->
            ( { model | tab = Nothing }, Cmd.none )

        OnChargeLateDues (Err error) ->
            ( { model | tab = Just <| AssignLateDues <| ErrorSending error }, Cmd.none )

        SendTransactionBatch ->
            case model.tab of
                Just (BatchTransactions batch _) ->
                    ( { model | tab = Just <| BatchTransactions batch Sending }
                    , createBatchOfTransactions model.common batch
                    )

                _ ->
                    ( model, Cmd.none )

        OnSendTransactionBatch (Ok _) ->
            ( { model | tab = Nothing }, Cmd.none )

        OnSendTransactionBatch (Err error) ->
            case model.tab of
                Just (BatchTransactions batch _) ->
                    ( { model | tab = Just <| BatchTransactions batch <| ErrorSending error }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



---- DATA ----


loadFees : Common -> Cmd Msg
loadFees common =
    getRequest common "/fees" (Decode.list feeDecoder)
        |> Task.attempt OnLoadFees


updateFeeAmount : Common -> Fee -> Cmd Msg
updateFeeAmount common fee =
    let
        url =
            "/fees/" ++ fee.name ++ "/" ++ String.fromInt fee.amount
    in
    postRequest common url (Encode.object [])
        |> Task.attempt OnUpdateFee


chargeDues : Common -> Cmd Msg
chargeDues common =
    postRequest common "/fees/charge_dues" (Encode.object [])
        |> Task.attempt OnChargeDues


chargeLateDues : Common -> Cmd Msg
chargeLateDues common =
    postRequest common "/fees/charge_late_dues" (Encode.object [])
        |> Task.attempt OnChargeLateDues


createBatchOfTransactions : Common -> TransactionBatch -> Cmd Msg
createBatchOfTransactions common batch =
    let
        body =
            Encode.object
                [ ( "members", Encode.list Encode.string batch.members )
                , ( "amount", batch.amount |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
                , ( "type", Encode.string batch.type_ )
                , ( "description", Encode.string batch.description )
                ]
    in
    postRequest common "/fees/create_batch" body
        |> Task.attempt OnSendTransactionBatch



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Dues"
        , Basics.box
            [ model.fees |> Basics.remoteContent feeList
            , Basics.submissionStateBox model.state
            ]
        , actionButtons
        , case model.tab of
            Nothing ->
                text ""

            Just (AssignDues state) ->
                assignDuesModal state

            Just (AssignLateDues state) ->
                assignLateDuesModal state

            Just (BatchTransactions batch state) ->
                batchTransactionsSidebar model.common batch state
        ]


actionButtons : Html Msg
actionButtons =
    let
        actionButton buttonText tab =
            button
                [ class "button is-primary"
                , onClick <| ChangeTab <| Just tab
                ]
                [ text buttonText ]

        allButtons =
            [ ( "Assign everyone dues", AssignDues NotSentYet )
            , ( "Make remaining dues late", AssignLateDues NotSentYet )
            , ( "Bake a batch of chocolate chip transactions", BatchTransactions emptyTransactionBatch NotSentYet )
            ]
    in
    ul []
        (allButtons
            |> List.map
                (\( name, tab ) ->
                    li [ style "margin-bottom" "10px" ]
                        [ actionButton name tab ]
                )
        )


feeList : List Fee -> Html Msg
feeList fees =
    table [ style "border-spacing" "5px", style "border-collapse" "separate" ]
        (fees |> List.map singleFee)


singleFee : Fee -> Html Msg
singleFee fee =
    tr []
        [ td [ style "padding-right" "10px" ]
            [ span [ style "display" "inline-block", style "vertical-align" "middle" ]
                [ label [ attribute "for" fee.name ] [ text fee.description ] ]
            ]
        , td []
            [ input
                [ type_ "number"
                , class "input"
                , attribute "name" fee.name
                , value <| String.fromInt fee.amount
                , onInput (UpdateFeeAmount fee)
                ]
                []
            ]
        ]


beholdThe : String -> Html Msg
beholdThe content =
    p [ style "margin-bottom" "10px" ]
        [ h3 [ class "subtitle is-3" ]
            [ text <| "Behold the " ++ content ]
        ]


cancelButton : Html Msg
cancelButton =
    a
        [ class "button is-pulled-right"
        , onClick <| ChangeTab Nothing
        ]
        [ text "ABORT! ABORT!" ]


assignDuesModal : SubmissionState -> Html Msg
assignDuesModal state =
    let
        chargeButton =
            a
                [ class <|
                    "button is-pulled-left is-primary"
                        ++ isLoadingClass (state == Sending)
                , onClick ChargeDues
                ]
                [ text "Dolla dolla bill, y'all" ]
    in
    Basics.modal (ChangeTab Nothing) <|
        div [ style "padding" "20px" ]
            [ h1 [ class "title" ]
                [ text "You are fixin' to assign dues to everyone "
                , b [] [ text "who has not yet been assigned dues this semester." ]
                ]
            , beholdThe "power"
            , beholdThe "corruption"
            , beholdThe "folksy phrasing"
            , case state of
                ErrorSending error ->
                    Basics.errorBox error

                _ ->
                    text ""
            , br [] []
            , chargeButton
            , cancelButton
            , br [] []
            ]


assignLateDuesModal : SubmissionState -> Html Msg
assignLateDuesModal state =
    let
        chargeButton =
            a
                [ class <|
                    "button is-pulled-left is-primary"
                        ++ isLoadingClass (state == Sending)
                , onClick ChargeLateDues
                ]
                [ text "Dolla dolla bill, y'all" ]
    in
    Basics.modal (ChangeTab Nothing) <|
        div [ style "padding" "20px" ]
            [ h1 [ class "title" ]
                [ text "You are fixin' to assign late dues to everyone "
                , b [] [ text "who has not yet paid their dues this semester." ]
                ]
            , beholdThe "power"
            , beholdThe "corruption"
            , beholdThe "folksy phrasing"
            , case state of
                ErrorSending error ->
                    Basics.errorBox error

                _ ->
                    text ""
            , br [] []
            , chargeButton
            , cancelButton
            , br [] []
            ]


batchTransactionsSidebar : Common -> TransactionBatch -> SubmissionState -> Html Msg
batchTransactionsSidebar common batch state =
    Basics.sidebar
        { data = Loaded ()
        , close = ChangeTab Nothing
        , render =
            \_ ->
                div []
                    [ Basics.backTextButton "cancel" (ChangeTab Nothing)
                    , Basics.title "Batch Transaction"
                    , selectInput
                        { title = "What's its persuasion?"
                        , helpText = Nothing
                        , values = common.info.transactionTypes
                        , render = \type_ -> ( type_, type_ )
                        , loading = False
                        , selected = (==) batch.type_
                        , onSelect = \type_ -> UpdateTransactionBatch { batch | type_ = type_ }
                        }
                    , textInput
                        { title = "What's it for?"
                        , helpText = Nothing
                        , value = batch.description
                        , placeholder = "Scotland Trip 2029"
                        , required = True
                        , onInput = \description -> UpdateTransactionBatch { batch | description = description }
                        }
                    , numberInputWithPrefix
                        { title = "How many doll hairs?"
                        , prefix = "$"
                        , helpText = Nothing
                        , value = batch.amount |> Maybe.map String.fromInt |> Maybe.withDefault ""
                        , placeholder = "420"
                        , required = True
                        , onInput = \amount -> UpdateTransactionBatch { batch | amount = amount |> String.toInt }
                        }
                    , fieldWrapper { title = "Whomdst?", helpText = Nothing } <|
                        [ Basics.box
                            [ ul [ style "column-count" "3", style "column-gap" "20px" ]
                                (common.members |> List.map (memberListItem batch))
                            ]
                        ]
                    , br [] []
                    , button
                        [ class <| "button is-primary" ++ isLoadingClass (state == Sending)
                        , onClick SendTransactionBatch
                        ]
                        [ text "My mind on my money and my money on my mind" ]
                    , case state of
                        ErrorSending error ->
                            Basics.errorBox error

                        _ ->
                            text ""
                    ]
        }


memberListItem : TransactionBatch -> Member -> Html Msg
memberListItem batch member =
    let
        memberName =
            member |> fullName

        onChange checked =
            UpdateTransactionBatch
                { batch
                    | members =
                        batch.members
                            |> List.filter ((/=) member.email)
                            |> List.append
                                (if checked then
                                    [ member.email ]

                                 else
                                    []
                                )
                }
    in
    li []
        [ checkboxInput
            { content = memberName
            , isChecked = batch.members |> List.any ((==) member.email)
            , onChange = onChange
            }
        ]
