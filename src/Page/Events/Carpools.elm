module Page.Events.Carpools exposing (viewCarpoolRow, viewCarpoolsList, viewEventCarpools)

import Html exposing (Html, b, br, div, li, text, ul)
import Models.Event exposing (EventCarpool, eventCarpoolDecoder)
import Page.Events exposing (Msg)
import Route exposing (Route)
import Utils exposing (RemoteData(..), spinner)



---- MODEL ----


type alias Model =
    { carpools : RemoteData (List EventCarpool)
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { carpools = Loading }, loadCarpools common eventId )



---- UPDATE ----


type Msg
    = OnLoadCarpools (Result Http.Error (List EventCarpool))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadCarpools (Ok carpools) ->
            ( { model | carpools = Loaded carpools }, Cmd.none )

        OnLoadCarpools (Err _) ->
            ( { model | carpools = Failure }, Cmd.none )



---- DATA ----


loadCarpools : Common -> Int -> Cmd Msg
loadCarpools common eventId =
    let
        url =
            "/events/" ++ String.fromInt eventId ++ "/carpools"
    in
    getRequest common url (Http.expectJson OnLoadCarpools <| Decode.list eventCarpoolDecoder)



---- VIEW ----


viewEventCarpools : RemoteData (List EventCarpool) -> Html Msg
viewEventCarpools carpools =
    let
        content =
            case carpools of
                NotRequestedYet ->
                    text ""

                Loading ->
                    spinner

                Loaded carpools ->
                    viewCarpoolsList carpools

                Error err ->
                    viewError err
    in
    div [ id "carpools" ] [ content ]


viewCarpoolsList : List EventCarpool -> Html Msg
viewCarpoolsList carpools =
    if List.length carpools == 0 then
        div [] [ text "No carpools set for this event." ]

    else
        ul [] List.map viewCarpoolRow carpools


viewCarpoolRow : EventCarpool -> Html Msg
viewCarpoolRow carpool =
    let
        passengerItem passenger =
            li [] [ text <| carpool.passenger.fullName ]

        passengerBlock =
            if List.length carpool.passengers == 0 then
                b [] [ text "themself", br [] [] ]

            else
                ul [] List.map passengerItem carpool.passengers
    in
    li [ key <| carpool.id ]
        [ b [] [ text carpool.driver.fullName ]
        , text "is driving"
        , passengerBlock
        , br [] []
        ]
