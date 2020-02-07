module Page.Admin.GigRequests exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Datetime exposing (dateFormatter, timeFormatter)
import Error exposing (GreaseResult)
import Html exposing (Html, br, button, div, i, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, style)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Admin exposing (GigRequest, GigRequestStatus(..), gigRequestDecoder)
import Route
import Task
import Time exposing (posixToMillis)
import Utils
    exposing
        ( Common
        , RemoteData(..)
        , SubmissionState(..)
        , checkSubmissionResult
        , getRequest
        , mapLoaded
        , postRequest
        , resultToRemote
        )



---- MODEL ----


type alias Model =
    { common : Common
    , requests : RemoteData (List GigRequest)
    , state : SubmissionState
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , requests = Loading
      , state = NotSentYet
      }
    , loadGigRequests common
    )



---- UPDATE ----


type Msg
    = OnLoadGigRequests (GreaseResult (List GigRequest))
    | OnRespondToGigRequest (GreaseResult ())
    | DismissGigRequest GigRequest
    | ReopenGigRequest GigRequest
    | CreateEventForGigRequest GigRequest
    | GoToEventForGigRequest Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadGigRequests result ->
            ( { model | requests = resultToRemote result }, Cmd.none )

        OnRespondToGigRequest result ->
            checkSubmissionResult model result

        DismissGigRequest request ->
            let
                updateRequest r =
                    { r
                        | status =
                            if r.id == request.id then
                                GigRequestDismissed

                            else
                                r.status
                    }
            in
            ( { model
                | state = Sending
                , requests = model.requests |> mapLoaded (List.map updateRequest)
              }
            , request |> dismissGigRequest model.common
            )

        ReopenGigRequest request ->
            let
                updateRequest r =
                    { r
                        | status =
                            if r.id == request.id then
                                GigRequestPending

                            else
                                r.status
                    }
            in
            ( { model
                | state = Sending
                , requests = model.requests |> mapLoaded (List.map updateRequest)
              }
            , request |> reopenGigRequest model.common
            )

        CreateEventForGigRequest request ->
            ( model, Route.loadPage <| Route.Admin (Just <| Route.AdminCreateEvent (Just request.id)) )

        GoToEventForGigRequest eventId ->
            ( model, Route.loadPage <| Route.Events { id = Just eventId, tab = Nothing } )



---- DATA ----


loadGigRequests : Common -> Cmd Msg
loadGigRequests common =
    getRequest common "/gig_requests?all=true" (Decode.list gigRequestDecoder)
        |> Task.attempt OnLoadGigRequests


dismissGigRequest : Common -> GigRequest -> Cmd Msg
dismissGigRequest common gigRequest =
    let
        url =
            "/gig_requests/" ++ String.fromInt gigRequest.id ++ "/dismiss"
    in
    postRequest common url (Encode.object [])
        |> Task.attempt OnRespondToGigRequest


reopenGigRequest : Common -> GigRequest -> Cmd Msg
reopenGigRequest common gigRequest =
    let
        url =
            "/gig_requests/" ++ String.fromInt gigRequest.id ++ "/reopen"
    in
    postRequest common url (Encode.object [])
        |> Task.attempt OnRespondToGigRequest



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        requests =
            model.requests
                |> mapLoaded (List.sortBy (.time >> posixToMillis))
    in
    div []
        [ Basics.title "Gig Requests"
        , Basics.box
            [ requests
                |> mapLoaded (List.filter (\r -> r.status == GigRequestPending))
                |> Basics.remoteContent (gigRequestTable model.common)
            , model.state |> Basics.submissionStateBox
            ]
        , Basics.title "Accepted Gig Requests"
        , Basics.box
            [ requests
                |> mapLoaded (List.filter (\r -> r.status == GigRequestAccepted))
                |> Basics.remoteContent (gigRequestTable model.common)
            ]
        , Basics.title "Dismissed Gig Requests"
        , Basics.box
            [ requests
                |> mapLoaded (List.filter (\r -> r.status == GigRequestDismissed))
                |> Basics.remoteContent (gigRequestTable model.common)
            ]
        ]


gigRequestTable : Common -> List GigRequest -> Html Msg
gigRequestTable common requests =
    table [ class "table", style "width" "100%" ]
        [ gigRequestHeader
        , tbody []
            (requests |> List.concatMap (singleGigRequest common))
        ]


gigRequestHeader : Html Msg
gigRequestHeader =
    let
        headers =
            [ "When Submitted", "Event Name", "Event Date", "Contact", "Description" ]
    in
    thead []
        [ tr [ style "width" "100%" ] (headers |> List.map (\h -> th [] [ text h ])) ]


singleGigRequest : Common -> GigRequest -> List (Html Msg)
singleGigRequest common gigRequest =
    [ tr
        [ class "no-bottom-border" ]
        [ td []
            [ text (gigRequest.time |> dateFormatter common.timeZone)
            , br [] []
            , text (gigRequest.time |> timeFormatter common.timeZone)
            ]
        , td []
            [ text gigRequest.name ]
        , td []
            [ text (gigRequest.startTime |> dateFormatter common.timeZone)
            , br [] []
            , text (gigRequest.startTime |> timeFormatter common.timeZone)
            , br [] []
            , text gigRequest.location
            ]
        , td []
            [ text gigRequest.organization
            , br [] []
            , text gigRequest.contactName
            , br [] []
            , Basics.phoneLink gigRequest.contactPhone
            , br [] []
            , Basics.emailLink gigRequest.contactEmail
            ]
        , td []
            [ i []
                [ text <|
                    "\""
                        ++ (gigRequest.comments |> Maybe.withDefault "no description given")
                        ++ "\""
                ]
            ]
        ]
    , tr [ class "no-bottom-border" ]
        [ td [ colspan 5 ]
            [ gigRequestButtons gigRequest ]
        ]
    ]


gigRequestButtons : GigRequest -> Html Msg
gigRequestButtons gigRequest =
    let
        ( leftButton, rightButton ) =
            case gigRequest.status of
                GigRequestPending ->
                    ( { content = "We do not deign"
                      , onClick = Just <| DismissGigRequest gigRequest
                      , attrs = []
                      }
                    , { content = "We deign"
                      , onClick = Just <| CreateEventForGigRequest gigRequest
                      , attrs = [ Buttons.Color Buttons.IsPrimary ]
                      }
                    )

                GigRequestAccepted ->
                    ( { content = "Too late to go back now"
                      , onClick = Nothing
                      , attrs = []
                      }
                    , { content = "We deigned"
                      , onClick = gigRequest.event |> Maybe.map GoToEventForGigRequest
                      , attrs = []
                      }
                    )

                GigRequestDismissed ->
                    ( { content = "We did not deign"
                      , onClick = Nothing
                      , attrs = []
                      }
                    , { content = "Hol up"
                      , onClick = Just <| ReopenGigRequest gigRequest
                      , attrs = []
                      }
                    )
    in
    Buttons.group
        { alignment = Buttons.AlignRight
        , connected = False
        , buttons =
            [ Buttons.button leftButton
            , Buttons.button rightButton
            ]
        }
