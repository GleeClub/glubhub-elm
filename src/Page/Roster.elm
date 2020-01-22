module Page.Roster exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Html exposing (Html, a, div, section, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, href)
import Models.Event exposing (Member)
import Route
import Utils exposing (Common, RemoteData(..), formatPhone, fullName)



---- MODEL ----


type alias Model =
    Common


init : Common -> ( Model, Cmd Msg )
init common =
    ( common, Cmd.none )



---- UPDATE ----


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ Basics.box
                [ memberTable model.members ]
            ]
        ]


memberTable : List Member -> Html Msg
memberTable members =
    let
        headers =
            [ "Name", "Section", "E-mail", "Phone", "Location" ]
    in
    table [ class "table is-fullwidth" ]
        [ thead []
            [ tr [] (headers |> List.map (\h -> td [] [ text h ])) ]
        , tbody [] (members |> List.map memberRow)
        ]


memberRow : Member -> Html Msg
memberRow member =
    tr []
        [ td [] [ a [ Route.href <| Route.Profile member.email ] [ text (member |> fullName) ] ]
        , td [] [ text <| Maybe.withDefault "None" member.section ]
        , td [] [ a [ href <| "mailto:" ++ member.email ] [ text member.email ] ]
        , td [] [ a [ href <| "tel:" ++ member.phoneNumber ] [ text <| formatPhone member.phoneNumber ] ]
        , td [] [ text member.location ]
        ]
