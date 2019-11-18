module Page.Profile exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Html exposing (Html, a, br, button, div, img, section, text)
import Html.Attributes exposing (class, href, id, src, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import List.Extra
import Maybe.Extra exposing (isJust)
import Models.Event exposing (Member, memberDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), formatPhone, getRequest, setToken, spinner)



---- MODEL ----


type alias Model =
    { common : Common
    , member : RemoteData Member
    }


init : Common -> String -> ( Model, Cmd Msg )
init common email =
    case common.members |> List.Extra.find (\member -> member.email == email) of
        Just member ->
            ( { common = common, member = Loaded member }, Cmd.none )

        Nothing ->
            ( { common = common, member = Loading }, loadMember common email )



---- UPDATE ----


type Msg
    = OnLoadMember (Result Http.Error Member)
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadMember (Ok member) ->
            ( { model | member = Loaded member }, Cmd.none )

        OnLoadMember (Err _) ->
            ( { model | member = Failure }, Cmd.none )

        Logout ->
            ( model, Cmd.batch [ setToken Nothing, Route.loadPage Route.Login ] )



---- DATA ----


loadMember : Common -> String -> Cmd Msg
loadMember common email =
    getRequest common ("/members/" ++ email) (Http.expectJson OnLoadMember memberDecoder)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        content =
            case model.member of
                NotAsked ->
                    text ""

                Loading ->
                    spinner

                Loaded member ->
                    viewProfile member (model.common.user |> Maybe.map (\u -> u.email == member.email) |> Maybe.withDefault False)

                Failure ->
                    text "Whoops..."
    in
    div [ id "home" ]
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "box" ] [ content ] ]
            ]
        ]


viewProfile : Member -> Bool -> Html Msg
viewProfile member isCurrentUser =
    let
        officership =
            if List.length member.positions > 0 then
                String.join ", " member.positions

            else
                "Not an officer"

        driverStatus =
            if member.passengers > 0 then
                String.fromInt member.passengers ++ " passengers"

            else
                "No car"

        arrivedAtTech =
            member.arrivedAtTech
                |> Maybe.map (\arrived -> "Arrived at tech in " ++ String.fromInt arrived)
                |> Maybe.withDefault "Came to Tech in the summer of '69"

        rows =
            [ text member.fullName
            , text officership
            , text (member.about |> Maybe.withDefault "I don't have a quote")
            , a [ href <| "mailto:" ++ member.email ] [ text member.email ]
            , a [ href <| "tel:" ++ member.phoneNumber ] [ text <| formatPhone member.phoneNumber ]
            , text member.location
            , text (member.major |> Maybe.withDefault "No major")
            , text driverStatus
            , text arrivedAtTech
            , text (member.section |> Maybe.withDefault "no section")
            ]
                ++ currentUserActions

        currentUserActions =
            if isCurrentUser then
                [ div []
                    [ a [ class "button", onClick Logout ] [ text "Log Out" ]
                    , Basics.linkButton "Edit" Route.EditProfile
                    ]
                ]

            else
                []
    in
    div [ class "columns" ]
        [ div [ class "column is-narrow" ]
            [ img [ src (member.picture |> Maybe.withDefault "http://lorempixel.com/g/256/256") ] [] ]
        , div [ class "column" ] (List.intersperse (br [] []) rows)
        ]
