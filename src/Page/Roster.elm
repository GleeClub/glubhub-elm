module Page.Roster exposing (Model, Msg(..), init, update, view, viewMemberTable)

import Html exposing (Html, a, div, h1, img, section, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, href, id, src)
import Http
import Json.Decode as Decode
import Models.Event exposing (Member, memberDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), apiUrl, notFoundView, spinner)



---- MODEL ----


type alias Model =
    { common : Common
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common }, Cmd.none )



---- UPDATE ----


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "roster" ]
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "box" ] [ viewMemberTable model.common.members ] ]
            ]
        ]


viewMemberTable : List Member -> Html Msg
viewMemberTable members =
    table [ class "table is-fullwidth" ]
        [ thead [] <|
            [ tr [] <|
                List.map (\h -> td [] [ text h ]) [ "Name", "Section", "E-mail", "Phone", "Location" ]
            ]
        , tbody [] <|
            (members
                |> List.map
                    (\m ->
                        tr []
                            [ td [] [ a [ Route.href <| Route.Profile m.email ] [ text m.fullName ] ]
                            , td [] [ text <| Maybe.withDefault "None" m.section ]
                            , td [] [ a [ href <| "mailto:" ++ m.email ] [ text m.email ] ]
                            , td [] [ a [ href <| "tel:" ++ m.phoneNumber ] [ text m.phoneNumber ] ]
                            , td [] [ text m.location ]
                            ]
                    )
            )
        ]
