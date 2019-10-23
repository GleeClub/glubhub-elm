module Main exposing (init, main, subscriptions)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Components.NavBar exposing (confirmAccountHeader, navBar)
import Html exposing (Html, a, div, section, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, maybe, nullable, string)
import Models.Info exposing (Info, infoDecoder, semesterDecoder)
import Models.Member exposing (Member, memberDecoder)
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
import Url exposing (Url)
import Utils exposing (Common, RemoteData(..), alert, apiUrl, handleJsonResponse)


type alias Model =
    { common : RemoteData Common
    , navKey : Key
    , route : Maybe Route
    , page : Page
    , burgerOpened : Bool
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
    | PageConfirmAccount Page.ConfirmAccount.Model
    | PageForgotPassword Page.ForgotPassword.Model
    | PageAdmin Page.Admin.Model


type Msg
    = OnFetchCommon (Result Http.Error Common)
    | LoadCurrentPage
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | ToggleBurger
    | HomeMsg Page.Home.Msg
    | LoginMsg Page.Login.Msg
    | RosterMsg Page.Roster.Msg
    | ProfileMsg Page.Profile.Msg
    | EditProfileMsg Page.EditProfile.Msg
    | EventsMsg Page.Events.Msg
    | RepertoireMsg Page.Repertoire.Msg
    | MinutesMsg Page.Minutes.Msg
    | ConfirmAccountMsg Page.ConfirmAccount.Msg
    | ForgotPasswordMsg Page.ForgotPassword.Msg
    | AdminMsg Page.Admin.Msg


init : Decode.Value -> Url -> Key -> ( Model, Cmd Msg )
init apiTokenJson url navKey =
    let
        token =
            Decode.decodeValue string apiTokenJson

        model =
            { common = Nothing
            , navKey = navKey
            , route = Just Route.Login
            , page = PageNone
            , burgerOpened = False
            }
    in
    case token of
        Err _ ->
            ( model, Cmd.none )

        Ok tokenString ->
            ( { model | route = Route.fromUrl url }, loadCommon tokenString navKey )


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
            getTask "/user" (Http.stringResolver <| handleJsonResponse memberDecoder)

        getMembers =
            getTask "/members" (Http.stringResolver <| handleJsonResponse <| Decode.list memberDecoder)

        getInfo =
            getTask "/static" (Http.stringResolver <| handleJsonResponse infoDecoder)

        getCurrentSemester =
            getTask "/semesters/current" (Http.stringResolver <| handleJsonResponse semesterDecoder)
    in
    Task.attempt OnFetchCommon <|
        Task.map4
            (\user members info currentSemester ->
                { user = user
                , members = members
                , info = info
                , currentSemester = currentSemester
                , token = token
                , key = key
                }
            )
            getUser
            getMembers
            getInfo
            getCurrentSemester


loadCurrentPage : Model -> ( Model, Cmd Msg )
loadCurrentPage model =
    case model.common of
        Nothing ->
            case model.page of
                PageLogin _ ->
                    ( model, Cmd.none )
                
                PageEditProfile _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | page = PageLogin pageModel }, Cmd.map LoginMsg pageCmd )

            let
                ( pageModel, pageCmd ) =
                    Page.Login.init model.navKey
            in
            ( { model | page = PageLogin pageModel }, Cmd.map LoginMsg pageCmd )

        -- ( PageNone, Cmd.none )
        -- ( model, cmd )
        Just common ->
            let
                ( page, newCmd ) =
                    case model.route of
                        Just Route.Roster ->
                            let
                                ( pageModel, pageCmd ) =
                                    Page.Roster.init common
                            in
                            ( PageRoster pageModel, Cmd.map RosterMsg pageCmd )
            in
            ( { model | page = page }, newCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( OnFetchCommon (Err _), _ ) ->
            ( model, alert "There was an error loading the page." )

        ( OnFetchCommon (Ok common), _ ) ->
            loadCurrentPage { model | common = Just common }

        ( OnUrlRequest urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( OnUrlChange url, _ ) ->
            loadCurrentPage { model | route = Route.fromUrl url }

        ( HomeMsg subMsg, PageHome pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.Home.update subMsg pageModel
            in
            ( { model | page = PageHome newPageModel }
            , Cmd.map HomeMsg newCmd
            )

        ( LoginMsg subMsg, PageLogin pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.Login.update subMsg pageModel
            in
            ( { model | page = PageLogin newPageModel }
            , Cmd.map LoginMsg newCmd
            )

        ( RosterMsg subMsg, PageRoster pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.Roster.update subMsg pageModel
            in
            ( { model | page = PageRoster newPageModel }
            , Cmd.map RosterMsg newCmd
            )

        ( ProfileMsg subMsg, PageProfile pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.Profile.update subMsg pageModel
            in
            ( { model | page = PageProfile newPageModel }
            , Cmd.map ProfileMsg newCmd
            )

        ( EditProfileMsg subMsg, PageEditProfile pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.EditProfile.update subMsg pageModel
            in
            ( { model | page = PageEditProfile newPageModel }
            , Cmd.map EditProfileMsg newCmd
            )

        ( EventsMsg subMsg, PageEvents pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.Events.update subMsg pageModel
            in
            ( { model | page = PageEvents newPageModel }
            , Cmd.map EventsMsg newCmd
            )

        ( RepertoireMsg subMsg, PageRepertoire pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.Repertoire.update subMsg pageModel
            in
            ( { model | page = PageRepertoire newPageModel }
            , Cmd.map RepertoireMsg newCmd
            )

        ( MinutesMsg subMsg, PageMinutes pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.Minutes.update subMsg pageModel
            in
            ( { model | page = PageMinutes newPageModel }
            , Cmd.map MinutesMsg newCmd
            )

        ( ConfirmAccountMsg subMsg, PageConfirmAccount pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.ConfirmAccount.update subMsg pageModel
            in
            ( { model | page = PageConfirmAccount newPageModel }
            , Cmd.map ConfirmAccountMsg newCmd
            )

        ( ForgotPasswordMsg subMsg, PageForgotPassword pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.ForgotPassword.update subMsg pageModel
            in
            ( { model | page = PageForgotPassword newPageModel }
            , Cmd.map ForgotPasswordMsg newCmd
            )

        ( AdminMsg subMsg, PageAdmin pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Page.Admin.update subMsg pageModel
            in
            ( { model | page = PageAdmin newPageModel }
            , Cmd.map AdminMsg newCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )


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
        ( common, content ) =
            case model.common of
                NotAsked ->
                    ( Nothing, currentPage model )

                Loading ->
                    ( Nothing, spinner )

                Loaded common ->
                    ( Just common, currentPage model )

                Failure ->
                    ( Nothing, text "We messed up real good..." )

        body =
            div [ id "app" ]
                [ navBar common model.burgerOpened
                , confirmAccountHeader common
                , content
                ]
    in
    { title = "App"
    , body = [ body ]
    }


currentPage : Model -> Html Msg
currentPage model =
    case model.page of
        PageHome pageModel ->
            Page.Home.view pageModel
                |> Html.map Home.Msg

        PageLogin pageModel ->
            Page.Login.view pageModel
                |> Html.map Login.Msg

        PageRoster pageModel ->
            Page.Roster.view pageModel
                |> Html.map Roster.Msg

        PageProfile pageModel ->
            Page.Profile.view pageModel
                |> Html.map Profile.Msg

        PageEditProfile pageModel ->
            Page.EditProfile.view pageModel
                |> Html.map EditProfile.Msg

        PageEvents pageModel ->
            Page.Events.view pageModel
                |> Html.map Events.Msg

        PageRepertoire pageModel ->
            Page.Repertoire.view pageModel
                |> Html.map Repertoire.Msg

        PageMinutes pageModel ->
            Page.Minutes.view pageModel
                |> Html.map Minutes.Msg

        PageConfirmAccount pageModel ->
            Page.ConfirmAccount.view pageModel
                |> Html.map ConfirmAccount.Msg

        PageForgotPassword pageModel ->
            Page.ForgotPassword.view pageModel
                |> Html.map ForgotPassword.Msg

        PageAdmin pageModel ->
            Page.Admin.view pageModel
                |> Html.map Admin.Msg
