module Page.Admin.Dues exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, form, h1, i, img, input, label, li, section, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, colspan, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import List.Extra exposing (groupWhile)
import Models.Admin exposing (Fee, feeDecoder)
import Models.Event exposing (EventAttendee, Member, eventAttendeeDecoder)
import Route exposing (Route)
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), getRequest, mapLoaded, postRequest, resultToRemote, resultToSubmissionState)



---- MODEL ----


type alias Model =
    { common : Common
    , fees : RemoteData (List Fee)
    , state : SubmissionState
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , fees = Loading
      , state = NotSentYet
      }
    , loadFees common
    )



---- UPDATE ----


type Msg
    = OnLoadFees (GreaseResult (List Fee))
    | UpdateFeeAmount String String
    | SendUpdateForFee Fee
    | OnUpdateFee (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadFees feesResult ->
            ( { model | fees = resultToRemote feesResult }, Cmd.none )

        SendUpdateForFee fee ->
            ( { model | state = Sending }, updateFeeAmount model.common fee )

        OnUpdateFee result ->
            ( { model | state = resultToSubmissionState result }, Cmd.none )

        UpdateFeeAmount feeName stringAmount ->
            case stringAmount |> String.toInt of
                Nothing ->
                    ( model, Cmd.none )

                Just amount ->
                    ( { model
                        | fees =
                            model.fees
                                |> mapLoaded
                                    (List.map
                                        (\fee ->
                                            if fee.name == feeName then
                                                { fee | amount = amount }

                                            else
                                                fee
                                        )
                                    )
                      }
                    , Cmd.none
                    )



---- DATA ----
-- (POST)   [/fees/(name: String)/apply] => apply_fee_for_all_active_members,


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



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Dues"
        , Basics.box
            [ model.fees |> Basics.remoteContent feeList
            , Basics.submissionStateBox model.state
            ]
        ]


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
                , onInput (UpdateFeeAmount fee.name)
                , onBlur (SendUpdateForFee fee)
                ]
                []
            ]
        ]
