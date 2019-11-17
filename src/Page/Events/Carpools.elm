module Page.Events.Carpools exposing (Model, Msg(..), init, loadCarpools, update, view, viewCarpoolRow, viewCarpoolsList)

import Html exposing (Html, b, br, div, li, text, ul)
import Html.Attributes exposing (id)
import Http
import Json.Decode as Decode
import Models.Event exposing (EventCarpool, eventCarpoolDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), getRequest, spinner)



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


view : Model -> Html Msg
view model =
    let
        content =
            case model.carpools of
                NotAsked ->
                    text ""

                Loading ->
                    spinner

                Loaded carpools ->
                    viewCarpoolsList carpools

                Failure ->
                    text "whoops"
    in
    div [ id "carpools" ] [ content ]


viewCarpoolsList : List EventCarpool -> Html Msg
viewCarpoolsList carpools =
    if List.length carpools == 0 then
        div [] [ text "No carpools set for this event." ]

    else
        ul [] <| List.map viewCarpoolRow carpools


viewCarpoolRow : EventCarpool -> Html Msg
viewCarpoolRow carpool =
    let
        passengers =
            carpool.passengers |> List.filter (\p -> p.email /= carpool.driver.email)

        passengerItem passenger =
            li [] [ text <| passenger.fullName ]

        passengerBlock =
            if List.length passengers == 0 then
                b [] [ text "themself", br [] [] ]

            else
                ul [] <| List.map passengerItem passengers
    in
    li []
        [ b [] [ text carpool.driver.fullName ]
        , text " is driving "
        , passengerBlock
        , br [] []
        ]
