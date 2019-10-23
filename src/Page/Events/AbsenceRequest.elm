module Page.Events.AbsenceRequest exposing (viewAbsenceRequest)

import Html exposing (Html, a, div, h1, img, section, table, tbody, td, text, textarea, thead, tr)
import Html.Attributes exposing (class, href, id, src, value)
import Html.Events exposing (onSubmit)
import Http
import Maybe.Extra
import Models.Event exposing (FullEvent)
import Page.Events exposing (Model, Msg)
import Route exposing (EventTab, Route, TabDetails)
import Time exposing (Posix)
import Utils exposing (apiUrl, postRequest)



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
            ( model, Cmd.none )

        OnSubmit (Ok _) ->
            ( model, Cmd.batch [ SwitchToDetailsTab, alert "Your request has been submitted. You lazy bum!" ] )

        OnSubmit (Err _) ->
            ( model, alert "We messed up submitting your request. Please be gentle..." )



---- DATA ----


submitAbsenceRequest : Model -> Cmd Msg
submitAbsenceRequest model =
    let
        url =
            "/absence_requests/" ++ String.fromInt model.eventId

        body =
            Http.jsonBody <| Encode.object [ ( "reason", Encode.string reason ) ]
    in
    postRequest common url body <| Http.expectWhatever OnSubmit



---- VIEW ----


viewAbsenceRequest reason =
    form [ id "absence-request", onSubmit Submit ]
        [ div [ class "field" ]
            [ label [ class "label" ] [ text "Reason" ]
            , div [ class "control" ]
                [ textarea []
                    [ class "textarea"
                    , value reason
                    , placeholder "My dog ate my homework."
                    ]
                ]
            ]
        , button [ type_ "button", class "button", onClick SwitchToDetailsTab ]
            [ text "jk nvm" ]
        , button [ type_ "submit", class "button is-primary" ]
            [ text "Beg for Mercy" ]
        ]
