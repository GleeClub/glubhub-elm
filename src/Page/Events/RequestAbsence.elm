module Page.Events.RequestAbsence exposing (Model, Msg(..), init, submitAbsenceRequest, switchToDetailsTab, update, view)

import Html exposing (Html, a, button, div, form, h1, img, label, section, table, tbody, td, text, textarea, thead, tr)
import Html.Attributes exposing (class, href, id, placeholder, src, type_, value)
import Html.Events exposing (onClick, onSubmit)
import Http
import Json.Encode as Encode
import Maybe.Extra
import Models.Event exposing (FullEvent)
import Route exposing (EventTab, Route)
import Time exposing (Posix)
import Utils exposing (Common, alert, apiUrl, postRequest)



---- MODEL ----


type alias Model =
    { common : Common
    , eventId : Int
    , reason : String
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { common = common, eventId = eventId, reason = "" }, Cmd.none )



---- UPDATE ----


type Msg
    = InputReason String
    | Submit
    | SwitchToDetailsTab
    | OnSubmit (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputReason reason ->
            ( { model | reason = reason }, Cmd.none )

        Submit ->
            ( model, submitAbsenceRequest model )

        SwitchToDetailsTab ->
            ( model, switchToDetailsTab model )

        OnSubmit (Ok _) ->
            ( model, Cmd.batch [ switchToDetailsTab model, alert "Your request has been submitted. You lazy bum!" ] )

        OnSubmit (Err _) ->
            ( model, alert "We messed up submitting your request. Please be gentle..." )



---- DATA ----


switchToDetailsTab : Model -> Cmd Msg
switchToDetailsTab model =
    Route.loadPage <| Route.Events { id = Just model.eventId, tab = Just Route.EventRequestAbsence }


submitAbsenceRequest : Model -> Cmd Msg
submitAbsenceRequest model =
    let
        url =
            "/absence_requests/" ++ String.fromInt model.eventId

        body =
            Http.jsonBody <| Encode.object [ ( "reason", Encode.string model.reason ) ]
    in
    postRequest model.common url body <| Http.expectWhatever OnSubmit



---- VIEW ----


view model =
    form [ id "absence-request", onSubmit Submit ]
        [ div [ class "field" ]
            [ label [ class "label" ] [ text "Reason" ]
            , div [ class "control" ]
                [ textarea
                    [ class "textarea"
                    , value model.reason
                    , placeholder "My dog ate my homework."
                    ]
                    []
                ]
            ]
        , button [ type_ "button", class "button", onClick SwitchToDetailsTab ]
            [ text "jk nvm" ]
        , button [ type_ "submit", class "button is-primary" ]
            [ text "Beg for Mercy" ]
        ]
