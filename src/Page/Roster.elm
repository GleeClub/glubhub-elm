module Page.Roster exposing (Model, Msg(..), init, loadMembers, update, view, viewMemberTable)

import Html exposing (Html, a, div, h1, img, section, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, href, id, src)
import Http
import Json.Decode as Decode
import Models.Member exposing (Member, memberDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), apiUrl, notFoundView, spinner)



---- MODEL ----


type alias Model =
    { members : RemoteData (List Member)
    , common : Common
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { members = Loading, common = common }, loadMembers common )



---- UPDATE ----


type Msg
    = OnFetchMembers (Result Http.Error (List Member))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        members =
            case msg of
                OnFetchMembers (Ok foundMembers) ->
                    Loaded foundMembers

                OnFetchMembers (Err error) ->
                    Failure
    in
    ( { model | members = members }, Cmd.none )



---- DATA ----


loadMembers : Common -> Cmd Msg
loadMembers common =
    Http.request
        { method = "GET"
        , url = apiUrl ++ "/members"
        , body = Http.emptyBody
        , headers = [ Http.header "token" common.token ]
        , expect = Http.expectJson OnFetchMembers (Decode.list <| memberDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        content =
            case model.members of
                NotAsked ->
                    text ""

                Loading ->
                    spinner

                Loaded members ->
                    viewMemberTable members

                Failure ->
                    text "Error"
    in
    div [ id "roster" ]
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "box" ] [ content ] ]
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
                            [ td [ Route.href Route.Home ] [ text m.fullName ]
                            , td [] [ text <| Maybe.withDefault "None" m.section ]
                            , td [] [ a [ href <| "mailto:" ++ m.email ] [ text m.email ] ]
                            , td [] [ a [ href <| "tel:" ++ m.phoneNumber ] [ text m.phoneNumber ] ]
                            , td [] [ text m.location ]
                            ]
                    )
            )
        ]
