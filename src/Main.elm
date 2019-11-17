module Main exposing (init, main, subscriptions)

-- import Page.ConfirmAccount

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, a, button, div, i, nav, section, span, text)
import Html.Attributes exposing (attribute, class, href, id, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, maybe, nullable, string)
import Maybe.Extra exposing (isJust, isNothing)
import Models.Event exposing (Member, memberDecoder)
import Models.Info exposing (Info, emptyInfo, emptySemester, infoDecoder, semesterDecoder)
import Page.Admin
import Page.EditProfile
import Page.Events
import Page.Home
import Page.Login
import Page.Minutes
import Page.Profile
import Page.Repertoire
import Page.Roster
import Route exposing (Route(..))
import Task
import Time exposing (Posix, here, now)
import Url exposing (Url)
import Utils exposing (Common, RemoteData(..), alert, apiUrl, handleJsonResponse, spinner)


type alias Model =
    { common : Maybe Common
    , navKey : Key
    , route : Maybe Route
    , page : Page
    , burgerOpened : Bool
    , ignoredConfirmPrompt : Bool
    }


type Page
    = PageNone
    | PageHome Page.Home.Model
    | PageLogin Page.Login.Model
    | PageRoster Page.Roster.Model
    | PageProfile Page.Profile.Model
    | PageEditProfile Page.EditProfile.Model
    | PageEvents Page.Events.Model
    | PageRepertoire Page.Repertoire.Model
    | PageMinutes Page.Minutes.Model
      -- | PageForgotPassword Page.ForgotPassword.Model
    | PageAdmin Page.Admin.Model


type Msg
    = OnFetchCommon (Result Http.Error Common)
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | ToggleBurger
    | IgnoreConfirmPrompt
    | Tick Posix
    | HomeMsg Page.Home.Msg
    | LoginMsg Page.Login.Msg
    | RosterMsg Page.Roster.Msg
    | ProfileMsg Page.Profile.Msg
    | EditProfileMsg Page.EditProfile.Msg
    | EventsMsg Page.Events.Msg
    | RepertoireMsg Page.Repertoire.Msg
    | MinutesMsg Page.Minutes.Msg
      -- | ForgotPasswordMsg Page.ForgotPassword.Msg
    | AdminMsg Page.Admin.Msg


init : Decode.Value -> Url -> Key -> ( Model, Cmd Msg )
init apiTokenJson url navKey =
    let
        token =
            Decode.decodeValue string apiTokenJson |> Result.withDefault ""

        model =
            { common = Nothing
            , navKey = navKey
            , route = Just Route.Login
            , page = PageNone
            , burgerOpened = False
            , ignoredConfirmPrompt = False
            }
    in
    ( { model | route = Route.fromUrl url }, loadCommon token navKey )


loadCommon : String -> Nav.Key -> Cmd Msg
loadCommon token key =
    let
        getTask url resolver =
            Http.task
                { method = "GET"
                , url = apiUrl ++ url
                , body = Http.emptyBody
                , headers = [ Http.header "token" token ]
                , resolver = resolver
                , timeout = Nothing
                }

        getUser =
            getTask "/user" (Http.stringResolver <| handleJsonResponse <| nullable memberDecoder)

        getMembers =
            if token == "" then
                Task.succeed []

            else
                getTask "/members" (Http.stringResolver <| handleJsonResponse <| Decode.list memberDecoder)

        getInfo =
            if token == "" then
                Task.succeed emptyInfo

            else
                getTask "/static" (Http.stringResolver <| handleJsonResponse infoDecoder)

        getCurrentSemester =
            if token == "" then
                Task.succeed emptySemester

            else
                getTask "/semesters/current" (Http.stringResolver <| handleJsonResponse semesterDecoder)

        getTimeAndTimeZone =
            Task.map2 (\time timeZone -> ( time, timeZone )) now here
    in
    Task.attempt OnFetchCommon <|
        Task.map5
            (\user members info currentSemester timeAndTimeZone ->
                { user = user
                , members = members
                , info = info
                , currentSemester = currentSemester
                , token = token
                , key = key
                , timeZone = Tuple.second timeAndTimeZone
                , now = Tuple.first timeAndTimeZone
                }
            )
            getUser
            getMembers
            getInfo
            getCurrentSemester
            getTimeAndTimeZone


loadCurrentPage : Model -> ( Model, Cmd Msg )
loadCurrentPage model =
    case model.common of
        Nothing ->
            ( model, Cmd.none )

        Just common ->
            case common.user of
                Nothing ->
                    case model.page of
                        PageLogin _ ->
                            ( model, Cmd.none )

                        PageEditProfile _ ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                ( pageModel, pageCmd ) =
                                    Page.Login.init
                            in
                            ( { model | page = PageLogin pageModel }, Cmd.batch [ Cmd.map LoginMsg pageCmd, Route.replaceUrl common.key Route.Login ] )

                Just user ->
                    case ( model.route, model.page ) of
                        ( Just Route.Login, _ ) ->
                            ( model, Route.replaceUrl common.key Route.Home )

                        ( Just Route.ForgotPassword, _ ) ->
                            ( model, Route.replaceUrl common.key Route.Home )

                        ( Just Route.Home, PageHome pageModel ) ->
                            ( model, Cmd.none )

                        ( Just Route.Home, _ ) ->
                            Page.Home.init common |> updateWith PageHome HomeMsg model

                        ( Just Route.Roster, PageRoster pageModel ) ->
                            ( model, Cmd.none )

                        ( Just Route.Roster, _ ) ->
                            Page.Roster.init common |> updateWith PageRoster RosterMsg model

                        ( Just (Route.Profile email), PageProfile pageModel ) ->
                            ( model, Cmd.none )

                        ( Just (Route.Profile email), _ ) ->
                            Page.Profile.init common email |> updateWith PageProfile ProfileMsg model

                        ( Just Route.EditProfile, PageEditProfile pageModel ) ->
                            ( model, Cmd.none )

                        ( Just Route.EditProfile, _ ) ->
                            Page.EditProfile.init common |> updateWith PageEditProfile EditProfileMsg model

                        ( Just (Route.Events route), PageEvents pageModel ) ->
                            ( model, Cmd.none )

                        ( Just (Route.Events route), _ ) ->
                            Page.Events.init common route |> updateWith PageEvents EventsMsg model

                        ( Just (Route.Repertoire songId), PageRepertoire pageModel ) ->
                            ( model, Cmd.none )

                        ( Just (Route.Repertoire songId), _ ) ->
                            Page.Repertoire.init common songId |> updateWith PageRepertoire RepertoireMsg model

                        ( Just (Route.Minutes route), PageMinutes pageModel ) ->
                            ( model, Cmd.none )

                        ( Just (Route.Minutes route), _ ) ->
                            Page.Minutes.init common route |> updateWith PageMinutes MinutesMsg model

                        ( Just (Route.Admin tab), PageAdmin pageModel ) ->
                            ( model, Cmd.none )

                        ( Just (Route.Admin tab), _ ) ->
                            Page.Admin.init common tab |> updateWith PageAdmin AdminMsg model

                        ( Nothing, _ ) ->
                            ( { model | page = PageNone }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (60 * 1000) Tick


urlRequest : Model -> UrlRequest -> ( Model, Cmd Msg )
urlRequest model request =
    case request of
        Browser.External url ->
            ( model
            , if String.isEmpty url then
                Cmd.none

              else
                Nav.load url
            )

        Browser.Internal url ->
            let
                route =
                    Route.fromUrl url

                urlChange =
                    model.common
                        |> Maybe.map .key
                        |> Maybe.map
                            (\key ->
                                Nav.pushUrl key
                                    (route
                                        |> Maybe.map Route.routeToString
                                        |> Maybe.withDefault ""
                                    )
                            )
                        |> Maybe.withDefault Cmd.none
            in
            loadCurrentPage { model | route = route }
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ urlChange, cmd ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( OnFetchCommon (Err _), _ ) ->
            ( model, alert "There was an error loading the page." )

        ( OnFetchCommon (Ok common), _ ) ->
            loadCurrentPage { model | common = Just common }

        ( OnUrlRequest request, _ ) ->
            urlRequest model request

        ( OnUrlChange url, _ ) ->
            loadCurrentPage { model | route = Route.fromUrl url }

        ( Tick time, _ ) ->
            case model.common of
                Just common ->
                    ( { model | common = Just { common | now = time } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( ToggleBurger, _ ) ->
            ( { model | burgerOpened = not model.burgerOpened }, Cmd.none )

        ( IgnoreConfirmPrompt, _ ) ->
            ( { model | ignoredConfirmPrompt = True }, Cmd.none )

        ( HomeMsg pageMsg, PageHome pageModel ) ->
            Page.Home.update pageMsg pageModel |> updateWith PageHome HomeMsg model

        ( HomeMsg _, _ ) ->
            ( model, Cmd.none )

        ( LoginMsg pageMsg, PageLogin pageModel ) ->
            Page.Login.update pageMsg pageModel |> updateWith PageLogin LoginMsg model

        ( LoginMsg _, _ ) ->
            ( model, Cmd.none )

        ( RosterMsg pageMsg, PageRoster pageModel ) ->
            Page.Roster.update pageMsg pageModel |> updateWith PageRoster RosterMsg model

        ( RosterMsg _, _ ) ->
            ( model, Cmd.none )

        ( ProfileMsg pageMsg, PageProfile pageModel ) ->
            Page.Profile.update pageMsg pageModel |> updateWith PageProfile ProfileMsg model

        ( ProfileMsg _, _ ) ->
            ( model, Cmd.none )

        ( EditProfileMsg pageMsg, PageEditProfile pageModel ) ->
            Page.EditProfile.update pageMsg pageModel |> updateWith PageEditProfile EditProfileMsg model

        ( EditProfileMsg _, _ ) ->
            ( model, Cmd.none )

        ( EventsMsg pageMsg, PageEvents pageModel ) ->
            Page.Events.update pageMsg pageModel |> updateWith PageEvents EventsMsg model

        ( EventsMsg _, _ ) ->
            ( model, Cmd.none )

        ( RepertoireMsg pageMsg, PageRepertoire pageModel ) ->
            Page.Repertoire.update pageMsg pageModel |> updateWith PageRepertoire RepertoireMsg model

        ( RepertoireMsg _, _ ) ->
            ( model, Cmd.none )

        ( MinutesMsg pageMsg, PageMinutes pageModel ) ->
            Page.Minutes.update pageMsg pageModel |> updateWith PageMinutes MinutesMsg model

        ( MinutesMsg _, _ ) ->
            ( model, Cmd.none )

        -- ( ForgotPasswordMsg pageMsg, PageForgotPassword pageModel ) ->
        --     Page.ForgotPassword.update pageMsg pageModel |> updateWith PageForgotPassword ForgotPasswordMsg model
        ( AdminMsg pageMsg, PageAdmin pageModel ) ->
            Page.Admin.update pageMsg pageModel |> updateWith PageAdmin AdminMsg model

        ( AdminMsg _, _ ) ->
            ( model, Cmd.none )


updateWith : (pageModel -> Page) -> (pageMsg -> Msg) -> Model -> ( pageModel, Cmd pageMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( pageModel, subCmd ) =
    ( { model | page = toModel pageModel }
    , Cmd.map toMsg subCmd
    )


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }



-- VIEWS


view : Model -> Browser.Document Msg
view model =
    let
        ( maybeUser, content ) =
            case model.common of
                Nothing ->
                    ( Nothing, div [ class "center" ] [ spinner ] )

                Just common ->
                    ( common.user, currentPage model )

        body =
            div [ id "app" ]
                [ navBar maybeUser model.burgerOpened
                , confirmAccountHeader (not model.ignoredConfirmPrompt && isJust model.common && isNothing maybeUser)
                , div [ style "padding-bottom" "50px" ] []
                , content
                ]
    in
    { title = "App"
    , body = [ body ]
    }


currentPage : Model -> Html Msg
currentPage model =
    case model.page of
        PageNone ->
            text ""

        PageHome pageModel ->
            Page.Home.view pageModel |> Html.map HomeMsg

        PageLogin pageModel ->
            Page.Login.view pageModel |> Html.map LoginMsg

        PageRoster pageModel ->
            Page.Roster.view pageModel |> Html.map RosterMsg

        PageProfile pageModel ->
            Page.Profile.view pageModel |> Html.map ProfileMsg

        PageEditProfile pageModel ->
            Page.EditProfile.view pageModel |> Html.map EditProfileMsg

        PageEvents pageModel ->
            Page.Events.view pageModel |> Html.map EventsMsg

        PageRepertoire pageModel ->
            Page.Repertoire.view pageModel |> Html.map RepertoireMsg

        PageMinutes pageModel ->
            Page.Minutes.view pageModel |> Html.map MinutesMsg

        -- PageForgotPassword pageModel ->
        --     Page.ForgotPassword.view pageModel |> Html.map ForgotPasswordMsg
        PageAdmin pageModel ->
            Page.Admin.view pageModel |> Html.map AdminMsg


navBar : Maybe Member -> Bool -> Html Msg
navBar maybeUser burgerOpened =
    nav [ class "navbar is-primary is-fixed-top", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ navBarLogoAndBurger burgerOpened
        , navBarLinks maybeUser
        ]


navBarLogoAndBurger : Bool -> Html Msg
navBarLogoAndBurger burgerOpened =
    div [ class "navbar-brand" ]
        [ a [ Route.href Route.Home, class "navbar-item" ]
            [ span [ class "icon is-small", style "width" "3vw" ]
                [ i [ class "fas fa-home" ] [] ]
            ]
        , a
            [ attribute "role" "button"
            , class <|
                "navbar-burger"
                    ++ (if burgerOpened then
                            " is-active"

                        else
                            ""
                       )
            , attribute "aria-label" "menu"
            , attribute "aria-expanded" "false"
            , onClick ToggleBurger
            ]
            [ span [ attribute "aria-hidden" "true" ] []
            , span [ attribute "aria-hidden" "true" ] []
            , span [ attribute "aria-hidden" "true" ] []
            ]
        ]


navBarLinks : Maybe Member -> Html Msg
navBarLinks maybeUser =
    let
        ( primaryLinks, profileLink ) =
            case maybeUser of
                Just user ->
                    ( [ a [ class "navbar-item", Route.href <| Route.Events { id = Nothing, tab = Nothing } ] [ text "Events" ]
                      , a [ class "navbar-item", Route.href <| Route.Repertoire Nothing ] [ text "Music" ]
                      , a [ class "navbar-item", Route.href Route.Roster ] [ text "People" ]
                      , a [ class "navbar-item", Route.href <| Route.Minutes { id = Nothing, tab = Nothing } ] [ text "Minutes" ]
                      , a [ class "navbar-item", Route.href <| Route.Admin Nothing ] [ text "Admin" ]
                      ]
                    , [ a
                            [ Route.href <| Route.Profile user.email
                            , class "navbar-item"
                            ]
                            [ text user.fullName ]
                      ]
                    )

                Nothing ->
                    ( [], [] )
    in
    div [ class "navbar-menu" ]
        [ div [ class "navbar-start" ] primaryLinks
        , div [ class "navbar-end" ] profileLink
        ]


confirmAccountHeader : Bool -> Html Msg
confirmAccountHeader showHeader =
    if not showHeader then
        section [ style "display" "none" ] []

    else
        section [ style "margin" "2em", style "margin-bottom" "-1em" ]
            [ div [ class "notification is-info" ]
                [ button [ class "delete", onClick IgnoreConfirmPrompt ] []
                , div
                    [ style "width" "100%"
                    , style "display" "flex"
                    , style "align-items" "center"
                    ]
                    [ div []
                        [ text "Welcome! Feel free to browse the site, but "
                        , text "if you're going to be active in Glee Club this "
                        , text "semester, please confirm your account so we "
                        , text "can get you into the system."
                        ]
                    , div []
                        [ a
                            [ Route.href Route.Home
                            , style "margin" "0 2em"
                            , class "button is-info is-inverted is-outlined"
                            ]
                            [ text "Confirm" ]
                        ]
                    ]
                ]
            ]
