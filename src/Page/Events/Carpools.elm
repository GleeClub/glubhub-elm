module Page.Events.Carpools exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, b, a, br, div, li, text, ul)
import Html.Attributes exposing (class, id)
import Http
import Json.Decode as Decode
import Models.Event exposing (EventCarpool, eventCarpoolDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), permittedTo, getRequest, spinner)
import Components.Basics as Basics


---- MODEL ----


type alias Model =
    { common : Common
    , eventId : Int
    , carpools : RemoteData (List EventCarpool)
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { common = common, eventId = eventId, carpools = Loading }, loadCarpools common eventId )


canEditCarpools : String
canEditCarpools =
    "edit-carpool"



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
        maybeEditButton =
            if model.common.user |> Maybe.map (permittedTo canEditCarpools) |> Maybe.withDefault False then
                Basics.linkButton "Edit Carpools" (Route.EditCarpools model.eventId)
            else
                text ""

        content =
            case model.carpools of
                NotAsked ->
                    text ""

                Loading ->
                    spinner

                Loaded carpools ->
                    div []
                        [ carpoolList carpools
                        , maybeEditButton
                        ]

                Failure ->
                    text "whoops"
    in
    div [ id "carpools" ] [ content ]


carpoolList : List EventCarpool -> Html Msg
carpoolList carpools =
    if List.length carpools == 0 then
        div [] [ text "No carpools set for this event." ]

    else
        ul [] <| List.map singleCarpool carpools


singleCarpool : EventCarpool -> Html Msg
singleCarpool carpool =
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
