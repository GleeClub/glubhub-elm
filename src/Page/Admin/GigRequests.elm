module Page.Admin.GigRequests exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Datetime exposing (dateFormatter, timeFormatter)
import Error exposing (GreaseResult)
import Html exposing (Html, a, br, button, div, i, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, disabled, href, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Admin exposing (GigRequest, GigRequestStatus(..), gigRequestDecoder)
import Route
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), checkSubmissionResult, formatPhone, getRequest, mapLoaded, postRequest, resultToRemote)



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

        openRequestFilter gigRequest =
            gigRequest.status == GigRequestPending

        closedRequestFilter gigRequest =
            gigRequest.status /= GigRequestPending
    in
    div []
        [ Basics.title "Open Gig Requests"
        , Basics.box
            [ requests
                |> mapLoaded (List.filter openRequestFilter)
                |> Basics.remoteContent (gigRequestTable model.common)
            ]
        , Basics.title "Closed Gig Requests"
        , Basics.box
            [ requests
                |> mapLoaded (List.filter closedRequestFilter)
                |> Basics.remoteContent (gigRequestTable model.common)
            , model.state |> Basics.submissionStateBox
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
            , a [ href <| "tel:" ++ gigRequest.contactPhone ]
                [ text <| formatPhone gigRequest.contactPhone ]
            , br [] []
            , a [ href <| "mailto:" ++ gigRequest.contactEmail ]
                [ text gigRequest.contactEmail ]
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
                    ( ( [], "We do not deign", Just (DismissGigRequest gigRequest) )
                    , ( [ class "oldgold" ], "We deign", Just (CreateEventForGigRequest gigRequest) )
                    )

                GigRequestAccepted ->
                    ( ( [], "Too late to go back now", Nothing )
                    , ( [], "We deigned", gigRequest.event |> Maybe.map GoToEventForGigRequest )
                    )

                GigRequestDismissed ->
                    ( ( [], "We did not deign", Nothing )
                    , ( [], "Hol up", Just (ReopenGigRequest gigRequest) )
                    )
    in
    div [ class "field is-grouped is-grouped-right" ]
        [ p [ class "control" ] [ gigRequestButton leftButton ]
        , p [ class "control" ] [ gigRequestButton rightButton ]
        ]


gigRequestButton : ( List (Html.Attribute Msg), String, Maybe Msg ) -> Html Msg
gigRequestButton ( attributes, content, maybeClickMsg ) =
    button
        ((class "button" :: attributes)
            ++ (maybeClickMsg
                    |> Maybe.map (\msg -> [ onClick msg ])
                    |> Maybe.withDefault [ disabled True, style "font-style" "italic" ]
               )
        )
        [ text content ]
