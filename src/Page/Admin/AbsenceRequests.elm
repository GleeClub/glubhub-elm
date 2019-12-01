module Page.Admin.AbsenceRequests exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, form, h1, h2, i, img, input, label, li, p, section, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, colspan, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import List.Extra exposing (getAt, groupWhile, updateAt)
import Models.Admin exposing (AbsenceRequest, AbsenceRequestState(..), absenceRequestDecoder)
import Models.Event exposing (Event, Member, eventDecoder)
import Route exposing (Route)
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), fullDateTimeFormatter, getRequest, mapLoaded, postRequest, resultToRemote, resultToSubmissionState)



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
            ( { model | state = resultToSubmissionState result }, Cmd.none )

        RespondToAbsenceRequest request approved ->
            case model.requestsAndEvents of
                Loaded requestsAndEvents ->
                    let
                        updateRequestAndEvents ( r, event ) =
                            if r.member == request.member && r.event == request.event then
                                ( { r
                                    | state =
                                        if approved then
                                            Approved

                                        else
                                            Denied
                                  }
                                , event
                                )

                            else
                                ( r, event )
                    in
                    ( { model
                        | state = Sending
                        , requestsAndEvents = Loaded (requestsAndEvents |> List.map updateRequestAndEvents)
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
    div []
        [ Basics.title "Absence Requests"
        , h2 [ class "subtitle", style "text-align" "center" ]
            [ text "for the semester" ]
        , Basics.box
            [ model.requestsAndEvents |> Basics.remoteContent (absenceRequestTable model.common)
            , model.state |> Basics.submissionStateBox
            ]
        ]


absenceRequestTable : Common -> List ( AbsenceRequest, Event ) -> Html Msg
absenceRequestTable common requestsAndEvents =
    let
        ( openRequests, closedRequests ) =
            requestsAndEvents |> List.partition (\( r, e ) -> r.state == Pending)

        openRequestRows =
            openRequests |> List.map (singleAbsenceRequest common)

        closedRequestRows =
            if List.isEmpty closedRequests then
                []

            else
                tr [ class "no-bottom-border" ] [ td [ colspan 6 ] [ Basics.divider "closed" ] ]
                    :: (closedRequests |> List.map (singleAbsenceRequest common))
    in
    table [ class "table" ]
        [ absenceRequestHeader
        , tbody []
            (openRequestRows ++ closedRequestRows)
        ]


absenceRequestHeader : Html Msg
absenceRequestHeader =
    thead []
        [ tr []
            [ th [] [ b [] [ text "Who?" ] ]
            , th [] [ b [] [ text "What?" ] ]
            , th [] [ b [] [ text "When?" ] ]
            , th [] [ b [] [ text "Where?" ] ]
            , th [] [ b [] [ text "Why?" ] ]
            , th [] [ b [] [ text "How (", i [] [ text "do you decide" ], text ")?" ] ]
            ]
        ]


singleAbsenceRequest : Common -> ( AbsenceRequest, Event ) -> Html Msg
singleAbsenceRequest common ( absenceRequest, event ) =
    tr
        ([ class "no-bottom-border" ]
            ++ (if absenceRequest.state /= Pending then
                    [ style "background-color" "#eee" ]

                else
                    []
               )
        )
        [ td []
            [ common.members
                |> List.Extra.find (\m -> m.email == absenceRequest.member)
                |> Maybe.map (\m -> a [ Route.href <| Route.Profile m.email ] [ text m.fullName ])
                |> Maybe.withDefault (i [] [ text "someone" ])
            ]
        , td []
            [ a
                [ Route.href <|
                    Route.Events { id = Just event.id, tab = Nothing }
                ]
                [ text event.name ]
            ]
        , td []
            [ text (event.callTime |> fullDateTimeFormatter common.timeZone) ]
        , td []
            [ event.location
                |> Maybe.map text
                |> Maybe.withDefault (i [] [ text "somewhere" ])
            ]
        , td [] [ i [] [ text <| "\"" ++ absenceRequest.reason ++ "\"" ] ]
        , td [] [ absenceRequestButtons absenceRequest ]
        ]


absenceRequestButtons : AbsenceRequest -> Html Msg
absenceRequestButtons absenceRequest =
    case absenceRequest.state of
        Pending ->
            div [ class "field is-grouped" ]
                [ p [ class "control" ]
                    [ button [ class "button is-success", onClick <| RespondToAbsenceRequest absenceRequest True ]
                        [ text "I pity the fool" ]
                    ]
                , p [ class "control" ]
                    [ button [ class "button is-danger", onClick <| RespondToAbsenceRequest absenceRequest False ]
                        [ text "Talk to the hand" ]
                    ]
                ]

        Approved ->
            div [ class "field" ]
                [ p [ style "padding-bottom" "5px" ]
                    [ i [ class "fas fa-thumbs-up" ] []
                    , text " I pitied the fool"
                    ]
                , p [ class "control" ]
                    [ button [ class "button", onClick <| RespondToAbsenceRequest absenceRequest False ]
                        [ text "Actually, I hate them" ]
                    ]
                ]

        Denied ->
            div [ class "field" ]
                [ p [ style "padding-bottom" "5px" ]
                    [ i [ class "fas fa-thumbs-down" ] []
                    , text " They talked to my hand"
                    ]
                , p [ class "control" ]
                    [ button [ class "button", onClick <| RespondToAbsenceRequest absenceRequest True ]
                        [ text "Spare the hand-talker" ]
                    ]
                ]
