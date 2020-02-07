module Page.Admin.Dues exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.Forms as Forms exposing (checkboxInput, inputWrapper, selectInput, textInput)
import Datetime
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, h1, h3, li, p, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Models.Admin exposing (Fee, feeDecoder)
import Models.Event exposing (Member)
import Models.Info exposing (Transaction, transactionDecoder)
import Request
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), fullName, mapLoaded, resultToRemote, resultToSubmissionState)



---- MODEL ----


type alias Model =
    { common : Common
    , fees : RemoteData (List Fee)
    , feeState : SubmissionState
    , transactions : RemoteData (List Transaction)
    , transactionState : SubmissionState
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
      , feeState = NotSentYet
      , transactions = Loading
      , transactionState = NotSentYet
      , tab = Nothing
      }
    , Cmd.batch [ loadFees common, loadTransactions common ]
    )



---- UPDATE ----


type Msg
    = OnLoadFees (GreaseResult (List Fee))
    | OnLoadTransactions (GreaseResult (List Transaction))
    | UpdateFeeAmount Fee Int
    | OnUpdateFee (GreaseResult ())
    | ChangeTab (Maybe DuesTab)
    | ChargeDues
    | OnChargeDues (GreaseResult ())
    | ChargeLateDues
    | OnChargeLateDues (GreaseResult ())
    | UpdateTransactionBatch TransactionBatch
    | SendTransactionBatch
    | OnSendTransactionBatch (GreaseResult ())
    | ResolveTransaction Int Bool
    | OnResolveTransaction (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadFees feesResult ->
            ( { model | fees = resultToRemote feesResult }, Cmd.none )

        OnLoadTransactions result ->
            ( { model | transactions = resultToRemote result }, Cmd.none )

        OnUpdateFee result ->
            ( { model | feeState = resultToSubmissionState result }, Cmd.none )

        OnResolveTransaction result ->
            ( { model | transactionState = resultToSubmissionState result }, Cmd.none )

        ChangeTab tab ->
            ( { model | tab = tab }, Cmd.none )

        UpdateTransactionBatch batch ->
            case model.tab of
                Just (BatchTransactions _ state) ->
                    ( { model | tab = Just <| BatchTransactions batch state }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateFeeAmount fee amount ->
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
                , feeState = Sending
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
            ( { model | tab = Nothing, transactions = Loading }, loadTransactions model.common )

        OnSendTransactionBatch (Err error) ->
            case model.tab of
                Just (BatchTransactions batch _) ->
                    ( { model | tab = Just <| BatchTransactions batch <| ErrorSending error }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ResolveTransaction transactionId resolved ->
            let
                mapper transaction =
                    if transaction.id == transactionId then
                        { transaction | resolved = resolved }

                    else
                        transaction
            in
            ( { model
                | transactions = model.transactions |> mapLoaded (List.map mapper)
                , transactionState = Sending
              }
            , resolveTransaction model.common transactionId resolved
            )



---- DATA ----


loadFees : Common -> Cmd Msg
loadFees common =
    Request.get common "/fees" (Decode.list feeDecoder)
        |> Task.attempt OnLoadFees


loadTransactions : Common -> Cmd Msg
loadTransactions common =
    Request.get common "/transactions" (Decode.list transactionDecoder)
        |> Task.attempt OnLoadTransactions


resolveTransaction : Common -> Int -> Bool -> Cmd Msg
resolveTransaction common transactionId resolved =
    let
        url =
            "/transactions/"
                ++ String.fromInt transactionId
                ++ "/resolve/"
                ++ (if resolved then
                        "true"

                    else
                        "false"
                   )
    in
    Request.post common url (Encode.object [])
        |> Task.attempt OnResolveTransaction


updateFeeAmount : Common -> Fee -> Cmd Msg
updateFeeAmount common fee =
    let
        url =
            "/fees/" ++ fee.name ++ "/" ++ String.fromInt fee.amount
    in
    Request.post common url (Encode.object [])
        |> Task.attempt OnUpdateFee


chargeDues : Common -> Cmd Msg
chargeDues common =
    Request.post common "/fees/charge_dues" (Encode.object [])
        |> Task.attempt OnChargeDues


chargeLateDues : Common -> Cmd Msg
chargeLateDues common =
    Request.post common "/fees/charge_late_dues" (Encode.object [])
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
    Request.post common "/fees/create_batch" body
        |> Task.attempt OnSendTransactionBatch



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Dues"
        , Basics.columns
            [ Basics.column
                [ Basics.box
                    [ model.fees |> Basics.remoteContent feeList
                    , Basics.submissionStateBox model.feeState
                    ]
                ]
            , Basics.column [ feeActionButtons ]
            ]
        , Basics.title "Transactions"
        , Basics.box
            [ model.transactions
                |> Basics.remoteContent (transactionTable model.common model.transactionState)
            ]
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


feeActionButtons : Html Msg
feeActionButtons =
    let
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
                        [ Buttons.button
                            { content = name
                            , onClick = Just <| ChangeTab <| Just tab
                            , attrs = [ Buttons.Color Buttons.IsPrimary ]
                            }
                        ]
                )
        )


feeList : List Fee -> Html Msg
feeList fees =
    div []
        (fees |> List.map singleFee)


singleFee : Fee -> Html Msg
singleFee fee =
    textInput Forms.int
        { value = Just fee.amount
        , onInput = \amount -> UpdateFeeAmount fee (amount |> Maybe.withDefault 0)
        , attrs = [ Forms.Horizontal, Forms.Title fee.name ]
        }


transactionTable : Common -> SubmissionState -> List Transaction -> Html Msg
transactionTable common state transactions =
    div []
        [ table [ class "table is-striped is-fullwidth" ]
            [ tbody []
                (transactions
                    |> List.sortBy (.time >> posixToMillis >> (*) -1)
                    |> List.map
                        (\transaction ->
                            tr [ class "no-bottom-border" ]
                                (transactionRow common transaction
                                    |> List.map (\cell -> td [] [ cell ])
                                )
                        )
                )
            ]
        , Basics.submissionStateBox state
        ]


transactionRow : Common -> Transaction -> List (Html Msg)
transactionRow common transaction =
    let
        ( statusText, actionButton ) =
            if transaction.resolved then
                ( "Resolved"
                , Buttons.button
                    { content = "Unresolve"
                    , onClick = Just <| ResolveTransaction transaction.id False
                    , attrs = [ Buttons.Size Buttons.Small ]
                    }
                )

            else
                ( "Outstanding"
                , Buttons.button
                    { content = "Resolve"
                    , onClick = Just <| ResolveTransaction transaction.id True
                    , attrs = [ Buttons.Size Buttons.Small, Buttons.Color Buttons.IsPrimary ]
                    }
                )
    in
    [ text (transaction.time |> Datetime.simpleDateWithYearFormatter common.timeZone)
    , transaction.member |> Utils.getMemberName common
    , text transaction.type_
    , text (transaction.amount |> String.fromInt)
    , text statusText
    , actionButton
    , text transaction.description
    ]


beholdThe : String -> Html Msg
beholdThe content =
    p [ style "margin-bottom" "10px" ]
        [ h3 [ class "subtitle is-3" ]
            [ text <| "Behold the " ++ content ]
        ]


cancelButton : Html Msg
cancelButton =
    Buttons.button
        { content = "ABORT! ABORT!"
        , onClick = Just <| ChangeTab Nothing
        , attrs =
            [ Buttons.CustomElement a
            , Buttons.CustomAttrs [ class "is-pulled-right" ]
            ]
        }


assignDuesModal : SubmissionState -> Html Msg
assignDuesModal state =
    let
        chargeButton =
            Buttons.button
                { content = "Dolla dolla bill, y'all"
                , onClick = Just ChargeDues
                , attrs =
                    [ Buttons.CustomElement a
                    , Buttons.CustomAttrs [ class "is-pulled-left" ]
                    , Buttons.Color Buttons.IsPrimary
                    , Buttons.IsLoading (state == Sending)
                    ]
                }
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
            Buttons.button
                { content = "Dolla dolla bill, y'all"
                , onClick = Just ChargeLateDues
                , attrs =
                    [ Buttons.CustomElement a
                    , Buttons.CustomAttrs [ class "is-pulled-left" ]
                    , Buttons.Color Buttons.IsPrimary
                    , Buttons.IsLoading (state == Sending)
                    ]
                }
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
                    [ Buttons.back
                        { content = "cancel"
                        , onClick = ChangeTab Nothing
                        }
                    , Basics.centeredTitle "Batch Transaction"
                    , selectInput Forms.string
                        { values = common.info.transactionTypes
                        , selected = batch.type_
                        , onInput = \type_ -> UpdateTransactionBatch { batch | type_ = type_ }
                        , attrs = [ Forms.Title "What's its persuasion?" ]
                        }
                    , textInput Forms.string
                        { value = batch.description
                        , onInput = \description -> UpdateTransactionBatch { batch | description = description }
                        , attrs =
                            [ Forms.Title "What's it for?"
                            , Forms.Placeholder "Scotland Trip 2029"
                            , Forms.RequiredField True
                            ]
                        }
                    , textInput Forms.int
                        { value = batch.amount
                        , onInput = \amount -> UpdateTransactionBatch { batch | amount = amount }
                        , attrs =
                            [ Forms.RequiredField True
                            , Forms.Placeholder "420"
                            , Forms.Prefix "$"
                            , Forms.Title "How many doll hairs?"
                            ]
                        }
                    , inputWrapper [ Forms.Title "Whomdst" ]
                        [ Basics.box
                            [ ul [ style "column-count" "3", style "column-gap" "20px" ]
                                (common.members |> List.map (memberListItem batch))
                            ]
                        ]
                    , br [] []
                    , Buttons.button
                        { content = "My mind on my money and my money on my mind"
                        , onClick = Just SendTransactionBatch
                        , attrs =
                            [ Buttons.Color Buttons.IsPrimary
                            , Buttons.IsLoading (state == Sending)
                            ]
                        }
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
                            |> List.remove member.email
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
