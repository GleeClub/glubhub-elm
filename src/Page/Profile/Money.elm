module Page.Profile.Money exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Datetime
import Error exposing (GreaseResult)
import Html exposing (Html, span, table, tbody, td, text, tr)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Event exposing (Member)
import Models.Info exposing (Enrollment(..), Transaction, transactionDecoder)
import Request
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), mapLoaded, resultToRemote)



---- MODEL ----


type alias Model =
    { common : Common
    , member : Member
    , transactions : RemoteData (List Transaction)
    , state : SubmissionState
    }


init : Common -> Member -> ( Model, Cmd Msg )
init common member =
    ( { common = common
      , member = member
      , state = NotSentYet
      , transactions = Loading
      }
    , loadMemberTransactions common member
    )



---- UPDATE ----


type Msg
    = OnLoadMemberTransactions (GreaseResult (List Transaction))
    | ResolveTransaction Int Bool
    | OnResolveTransaction (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadMemberTransactions result ->
            ( { model | transactions = resultToRemote result }, Cmd.none )

        ResolveTransaction transactionId resolved ->
            let
                transactionMapper transaction =
                    if transaction.id == transactionId then
                        { transaction | resolved = resolved }

                    else
                        transaction
            in
            ( { model
                | state = Sending
                , transactions = model.transactions |> mapLoaded (List.map transactionMapper)
              }
            , resolveTransaction model.common transactionId resolved
            )

        OnResolveTransaction result ->
            Utils.checkSubmissionResult model result



---- DATA ----


loadMemberTransactions : Common -> Member -> Cmd Msg
loadMemberTransactions common member =
    let
        url =
            "/transactions/" ++ member.email
    in
    Request.get common url (Decode.list transactionDecoder)
        |> Task.attempt OnLoadMemberTransactions


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



---- VIEW ----


view : Model -> Html Msg
view model =
    Basics.column
        [ model.transactions
            |> Basics.remoteContent (profileMoney model.common)
        , Basics.submissionStateBox model.state
        ]


profileMoney : Common -> List Transaction -> Html Msg
profileMoney common transactions =
    table [ class "table is-striped" ]
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


transactionRow : Common -> Transaction -> List (Html Msg)
transactionRow common transaction =
    let
        summary =
            transaction.type_
                ++ (if String.isEmpty transaction.description then
                        ""

                    else
                        " (" ++ transaction.description ++ ")"
                   )

        amount =
            if transaction.amount < 0 then
                span
                    [ style "color" "green" ]
                    [ text <|
                        "("
                            ++ (transaction.amount
                                    |> (*) -1
                                    |> String.fromInt
                               )
                            ++ ")"
                    ]

            else
                text (transaction.amount |> String.fromInt)

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
    , text summary
    , amount
    , text statusText
    , actionButton
    ]
