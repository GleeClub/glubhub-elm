module Page.Events.Carpools exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Error exposing (GreaseResult)
import Html exposing (Html, b, br, div, li, text, ul)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Models.Event exposing (EventCarpool, eventCarpoolDecoder)
import Permissions
import Route
import Task
import Utils exposing (Common, RemoteData(..), fullName, getRequest, resultToRemote)



---- MODEL ----


type alias Model =
    { common : Common
    , eventId : Int
    , carpools : RemoteData (List EventCarpool)
    }


init : Common -> Int -> ( Model, Cmd Msg )
init common eventId =
    ( { common = common, eventId = eventId, carpools = Loading }, loadCarpools common eventId )



---- UPDATE ----


type Msg
    = OnLoadCarpools (GreaseResult (List EventCarpool))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadCarpools carpoolsResult ->
            ( { model | carpools = resultToRemote carpoolsResult }, Cmd.none )



---- DATA ----


loadCarpools : Common -> Int -> Cmd Msg
loadCarpools common eventId =
    let
        url =
            "/events/" ++ String.fromInt eventId ++ "/carpools"
    in
    getRequest common url (Decode.list eventCarpoolDecoder)
        |> Task.attempt OnLoadCarpools



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        render carpools =
            div []
                [ carpoolList carpools
                , div [ style "padding" "10px" ]
                    [ Basics.renderIfHasPermission model.common Permissions.editCarpool <|
                        Buttons.link
                            { content = "Edit Carpools"
                            , route = Route.EditCarpools model.eventId
                            , attrs = []
                            }
                    ]
                ]
    in
    model.carpools |> Basics.remoteContent render


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
            li [] [ text (passenger |> fullName) ]

        passengerBlock =
            if List.isEmpty passengers then
                b [] [ text "themself", br [] [] ]

            else
                ul [] <| List.map passengerItem passengers
    in
    li []
        [ b [] [ text (carpool.driver |> fullName) ]
        , text " is driving "
        , passengerBlock
        , br [] []
        ]
