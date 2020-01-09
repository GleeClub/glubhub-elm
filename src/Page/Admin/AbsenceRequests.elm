module Page.Admin.AbsenceRequests exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Datetime exposing (dateFormatter, timeFormatter)
import Error exposing (GreaseResult)
import Html exposing (Html, a, br, button, div, i, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, disabled, href, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Models.Admin exposing (AbsenceRequest, AbsenceRequestState(..), absenceRequestDecoder)
import Models.Event exposing (Event, eventDecoder)
import Route
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), checkSubmissionResult, fullName, getRequest, mapLoaded, postRequest, resultToRemote)



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
                        updateRequestsAndEvents r =
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
                                    |> List.map (Tuple.mapFirst updateRequestsAndEvents)
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
    getRequest common "/absence_requests" (Decode.list decodeTuple)
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
            [ "/absence_requests", String.fromInt absenceRequest.event, absenceRequest.member, action ]
                |> String.join "/"
    in
    postRequest common url (Encode.object [])
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
    [ tr
        [ class "no-bottom-border" ]
        [ td []
            [ text (absenceRequest.time |> dateFormatter common.timeZone)
            , br [] []
            , text (absenceRequest.time |> timeFormatter common.timeZone)
            ]
        , td []
            [ a
                [ Route.href <|
                    Route.Events { id = Just event.id, tab = Nothing }
                ]
                [ text event.name ]
            ]
        , td []
            [ text (event.callTime |> dateFormatter common.timeZone)
            , br [] []
            , text (event.callTime |> timeFormatter common.timeZone)
            , br [] []
            , text (event.location |> Maybe.withDefault "")
            ]
        , td []
            [ common.members
                |> List.Extra.find (\m -> m.email == absenceRequest.member)
                |> Maybe.map (\m -> a [ Route.href <| Route.Profile m.email ] [ text (m |> fullName) ])
                |> Maybe.withDefault (i [] [ text absenceRequest.member ])
            ]
        , td [] [ i [] [ text <| "\"" ++ absenceRequest.reason ++ "\"" ] ]
        ]
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
                    ( ( [], "Get fukt nerd", Just False )
                    , ( [ class "oldgold" ], "Bestow mercy", Just True )
                    )

                AbsenceRequestApproved ->
                    ( ( [], "Jk get fukt nerd", Just False )
                    , ( [], "Mercy bestowed", Nothing )
                    )

                AbsenceRequestDenied ->
                    ( ( [], "Nerd got fukt", Nothing )
                    , ( [ style "white-space" "normal"
                        , style "max-width" "150px"
                        , style "height" "initial"
                        ]
                      , "I have heard your pleas and acquiesced to your request"
                      , Just True
                      )
                    )
    in
    div [ class "field is-grouped is-grouped-right" ]
        [ p [ class "control" ] [ absenceRequestButton absenceRequest leftButton ]
        , p [ class "control" ] [ absenceRequestButton absenceRequest rightButton ]
        ]


absenceRequestButton : AbsenceRequest -> ( List (Html.Attribute Msg), String, Maybe Bool ) -> Html Msg
absenceRequestButton absenceRequest ( attributes, content, maybeClickMsg ) =
    button
        ((class "button" :: attributes)
            ++ (maybeClickMsg
                    |> Maybe.map (\approved -> [ onClick <| RespondToAbsenceRequest absenceRequest approved ])
                    |> Maybe.withDefault [ disabled True, style "font-style" "italic" ]
               )
        )
        [ text content ]
