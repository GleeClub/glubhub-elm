module Page.Admin.AbsenceRequests exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Datetime exposing (dateFormatter, timeFormatter)
import Error exposing (GreaseResult)
import Html exposing (Html, a, br, button, div, i, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, href, style)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Admin exposing (AbsenceRequest, AbsenceRequestState(..), absenceRequestDecoder)
import Models.Event exposing (Event, eventDecoder)
import Request
import Route
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), checkSubmissionResult, mapLoaded, resultToRemote)



---- MODEL ----


type alias Model =
    { common : Common
    , requestsAndEvents : RemoteData (List ( AbsenceRequest, Event ))
    , state : SubmissionState
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , requestsAndEvents = Loading
      , state = NotSentYet
      }
    , loadAbsenceRequests common
    )



---- UPDATE ----


type Msg
    = OnLoadAbsenceRequests (GreaseResult (List ( AbsenceRequest, Event )))
    | RespondToAbsenceRequest AbsenceRequest Bool
    | OnRespondToAbsenceRequest (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadAbsenceRequests result ->
            ( { model | requestsAndEvents = resultToRemote result }, Cmd.none )

        OnRespondToAbsenceRequest result ->
            checkSubmissionResult model result

        RespondToAbsenceRequest request approved ->
            case model.requestsAndEvents of
                Loaded requestsAndEvents ->
                    let
                        updateRequests r =
                            { r
                                | state =
                                    if r.member == request.member && r.event == request.event then
                                        if approved then
                                            AbsenceRequestApproved

                                        else
                                            AbsenceRequestDenied

                                    else
                                        r.state
                            }
                    in
                    ( { model
                        | state = Sending
                        , requestsAndEvents =
                            Loaded
                                (requestsAndEvents
                                    |> List.map (Tuple.mapFirst updateRequests)
                                )
                      }
                    , request |> respondToAbsenceRequest model.common approved
                    )

                _ ->
                    ( model, Cmd.none )



---- DATA ----


loadAbsenceRequests : Common -> Cmd Msg
loadAbsenceRequests common =
    let
        decodeTuple =
            Decode.map2 Tuple.pair
                (Decode.index 0 absenceRequestDecoder)
                (Decode.index 1 eventDecoder)
    in
    Request.get common "/absence_requests" (Decode.list decodeTuple)
        |> Task.attempt OnLoadAbsenceRequests


respondToAbsenceRequest : Common -> Bool -> AbsenceRequest -> Cmd Msg
respondToAbsenceRequest common approved absenceRequest =
    let
        action =
            if approved then
                "approve"

            else
                "deny"

        url =
            [ "/absence_requests"
            , String.fromInt absenceRequest.event
            , absenceRequest.member
            , action
            ]
                |> String.join "/"
    in
    Request.post common url (Encode.object [])
        |> Task.attempt OnRespondToAbsenceRequest



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        requestsAndEvents =
            model.requestsAndEvents
                |> mapLoaded (List.sortBy (Tuple.first >> .time >> posixToMillis))

        openRequestFilter ( absenceRequest, _ ) =
            absenceRequest.state == AbsenceRequestPending

        closedRequestFilter ( absenceRequest, _ ) =
            absenceRequest.state /= AbsenceRequestPending
    in
    div [ style "width" "100%" ]
        [ Basics.title "Open Absence Requests"
        , Basics.box
            [ requestsAndEvents
                |> mapLoaded (List.filter openRequestFilter)
                |> Basics.remoteContent (absenceRequestTable model.common)
            ]
        , Basics.title "Closed Absence Requests"
        , Basics.box
            [ requestsAndEvents
                |> mapLoaded (List.filter closedRequestFilter)
                |> Basics.remoteContent (absenceRequestTable model.common)
            , model.state |> Basics.submissionStateBox
            ]
        ]


absenceRequestTable : Common -> List ( AbsenceRequest, Event ) -> Html Msg
absenceRequestTable common requestsAndEvents =
    table [ class "table", style "width" "100%" ]
        [ absenceRequestHeader
        , tbody []
            (requestsAndEvents |> List.concatMap (singleAbsenceRequest common))
        ]


absenceRequestHeader : Html Msg
absenceRequestHeader =
    let
        headers =
            [ "When Submitted", "Event Name", "Event Date", "Loser", "Excuse" ]
    in
    thead []
        [ tr [ style "width" "100%" ] (headers |> List.map (\h -> th [] [ text h ])) ]


singleAbsenceRequest : Common -> ( AbsenceRequest, Event ) -> List (Html Msg)
singleAbsenceRequest common ( absenceRequest, event ) =
    let
        infoRow =
            [ [ text (absenceRequest.time |> dateFormatter common.timeZone)
              , br [] []
              , text (absenceRequest.time |> timeFormatter common.timeZone)
              ]
            , [ a
                    [ Route.href <|
                        Route.Events { id = Just event.id, tab = Nothing }
                    ]
                    [ text event.name ]
              ]
            , [ text (event.callTime |> dateFormatter common.timeZone)
              , br [] []
              , text (event.callTime |> timeFormatter common.timeZone)
              , br [] []
              , text (event.location |> Maybe.withDefault "")
              ]
            , [ a
                    [ Route.href <|
                        Route.Profile { email = absenceRequest.member, tab = Nothing }
                    ]
                    [ Utils.getMemberName common absenceRequest.member ]
              ]
            , [ i [] [ text <| "\"" ++ absenceRequest.reason ++ "\"" ] ]
            ]
    in
    [ tr
        [ class "no-bottom-border" ]
        (infoRow |> List.map (td []))
    , tr [ class "no-bottom-border" ]
        [ td [ colspan 5 ]
            [ absenceRequestButtons absenceRequest ]
        ]
    ]


absenceRequestButtons : AbsenceRequest -> Html Msg
absenceRequestButtons absenceRequest =
    let
        ( leftButton, rightButton ) =
            case absenceRequest.state of
                AbsenceRequestPending ->
                    ( { content = "Get fukt nerd"
                      , onClick = Just <| RespondToAbsenceRequest absenceRequest False
                      , attrs = []
                      }
                    , { content = "Bestow mercy"
                      , onClick = Just <| RespondToAbsenceRequest absenceRequest True
                      , attrs = [ Buttons.Color Buttons.IsPrimary ]
                      }
                    )

                AbsenceRequestApproved ->
                    ( { content = "Jk get fukt nerd"
                      , onClick = Just <| RespondToAbsenceRequest absenceRequest False
                      , attrs = []
                      }
                    , { content = "Mercy bestowed"
                      , onClick = Nothing
                      , attrs = []
                      }
                    )

                AbsenceRequestDenied ->
                    ( { content = "Nerd got fukt"
                      , onClick = Nothing
                      , attrs = []
                      }
                    , { content = "I have heard your pleas and acquiesced to your request"
                      , onClick = Just <| RespondToAbsenceRequest absenceRequest True
                      , attrs =
                            [ Buttons.CustomAttrs
                                [ style "white-space" "normal"
                                , style "max-width" "150px"
                                , style "height" "initial"
                                ]
                            ]
                      }
                    )
    in
    Buttons.group
        { alignment = Buttons.AlignRight
        , connected = False
        , buttons =
            [ leftButton, rightButton ]
                |> List.map Buttons.button
        }
