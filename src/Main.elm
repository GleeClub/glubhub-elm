module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Components.Basics as Basics
import Components.ConfirmAccount as ConfirmAccount exposing (confirmAccountHeader)
import Components.NavBar exposing (navBar)
import Error exposing (GreaseResult)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id, style)
import Json.Decode as Decode exposing (nullable, string)
import Maybe.Extra exposing (isJust, isNothing)
import Models.Event exposing (memberDecoder)
import Models.Info exposing (infoDecoder, semesterDecoder)
import Page.Admin
import Page.EditProfile
import Page.Events
import Page.Events.EditCarpools as EditCarpools
import Page.ForgotPassword
import Page.Home
import Page.Login
import Page.Minutes
import Page.NotFound
import Page.Profile
import Page.Repertoire
import Page.ResetPassword
import Page.Roster
import Route exposing (Route(..))
import Task
import Time exposing (Posix, here, now)
import Url exposing (Url)
import Utils exposing (Common, RemoteData(..), getRequest, mapLoaded, remoteToMaybe, setToken)


type alias Model =
    { common : RemoteData Common
    , navKey : Key
    , route : Maybe Route
    , page : Maybe Page
    , burgerOpened : Bool
    , ignoredConfirmPrompt : Bool
    , confirmAccountModal : Maybe ConfirmAccount.Model
    }


type Page
    = PageNotFound Common
    | PageHome Page.Home.Model
    | PageLogin Page.Login.Model
    | PageRoster Page.Roster.Model
    | PageProfile Page.Profile.Model
    | PageEditProfile Page.EditProfile.Model
    | PageEvents Page.Events.Model
    | PageEditCarpools EditCarpools.Model
    | PageRepertoire Page.Repertoire.Model
    | PageMinutes Page.Minutes.Model
    | PageForgotPassword Page.ForgotPassword.Model
    | PageResetPassword Page.ResetPassword.Model
    | PageAdmin Page.Admin.Model


type Msg
    = OnFetchCommon (GreaseResult Common)
    | Tick Posix
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | ToggleBurger
    | IgnoreConfirmPrompt
    | ConfirmAccount
    | CancelConfirmAccount
    | ConfirmAccountMsg ConfirmAccount.Msg
    | HomeMsg Page.Home.Msg
    | LoginMsg Page.Login.Msg
    | RosterMsg Page.Roster.Msg
    | ProfileMsg Page.Profile.Msg
    | EditProfileMsg Page.EditProfile.Msg
    | EventsMsg Page.Events.Msg
    | EditCarpoolsMsg EditCarpools.Msg
    | RepertoireMsg Page.Repertoire.Msg
    | MinutesMsg Page.Minutes.Msg
    | ForgotPasswordMsg Page.ForgotPassword.Msg
    | ResetPasswordMsg Page.ResetPassword.Msg
    | AdminMsg Page.Admin.Msg


init : Decode.Value -> Url -> Key -> ( Model, Cmd Msg )
init apiTokenJson url navKey =
    let
        token =
            Decode.decodeValue string apiTokenJson |> Result.withDefault ""

        model =
            { common = Loading
            , navKey = navKey
            , route = Just Route.Login
            , page = Nothing
            , burgerOpened = False
            , ignoredConfirmPrompt = False
            , confirmAccountModal = Nothing
            }
    in
    ( { model | route = Route.fromUrl url }
    , loadCommon token navKey
    )


loadCommon : String -> Nav.Key -> Cmd Msg
loadCommon token key =
    let
        auth =
            { token = token }

        getUser =
            getRequest auth "/user" (nullable memberDecoder)

        getMembers =
            if token == "" then
                Task.succeed []

            else
                getRequest auth "/members" (Decode.list memberDecoder)

        getInfo =
            getRequest auth "/static" infoDecoder

        getCurrentSemester =
            getRequest auth "/semesters/current" semesterDecoder

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
    case model.common |> remoteToMaybe of
        Nothing ->
            ( model, Cmd.none )

        Just common ->
            if common.user |> isNothing then
                let
                    loadUnauthorizedPage pageInit page msg route =
                        let
                            ( pageModel, pageCmd ) =
                                pageInit
                        in
                        ( { model | page = Just <| page pageModel }
                        , Cmd.batch
                            [ Cmd.map msg pageCmd
                            , Route.replaceUrl common.key route
                            ]
                        )
                in
                case ( model.route, model.page ) of
                    ( Just Route.EditProfile, Just (PageEditProfile _) ) ->
                        ( model, Cmd.none )

                    ( Just Route.Login, Just (PageLogin _) ) ->
                        ( model, Cmd.none )

                    ( Just Route.ForgotPassword, Just (PageForgotPassword _) ) ->
                        ( model, Cmd.none )

                    ( Just (Route.ResetPassword _), Just (PageResetPassword _) ) ->
                        ( model, Cmd.none )

                    ( Just Route.EditProfile, _ ) ->
                        loadUnauthorizedPage (Page.EditProfile.init common) PageEditProfile EditProfileMsg Route.EditProfile

                    ( Just Route.ForgotPassword, _ ) ->
                        loadUnauthorizedPage Page.ForgotPassword.init PageForgotPassword ForgotPasswordMsg Route.ForgotPassword

                    ( Just (Route.ResetPassword token), _ ) ->
                        loadUnauthorizedPage (Page.ResetPassword.init token) PageResetPassword ResetPasswordMsg (Route.ResetPassword token)

                    _ ->
                        loadUnauthorizedPage Page.Login.init PageLogin LoginMsg Route.Login

            else
                case ( model.route, model.page ) of
                    ( Just Route.Login, _ ) ->
                        ( model, Route.replaceUrl common.key Route.Home )

                    ( Just Route.ForgotPassword, Just (PageForgotPassword _) ) ->
                        ( model, Cmd.none )

                    ( Just Route.ForgotPassword, _ ) ->
                        Page.ForgotPassword.init |> updateWith PageForgotPassword ForgotPasswordMsg model

                    ( Just (Route.ResetPassword _), Just (PageResetPassword _) ) ->
                        ( model, Cmd.none )

                    ( Just (Route.ResetPassword token), _ ) ->
                        Page.ResetPassword.init token |> updateWith PageResetPassword ResetPasswordMsg model

                    ( Just Route.Home, Just (PageHome _) ) ->
                        ( model, Cmd.none )

                    ( Just Route.Home, _ ) ->
                        Page.Home.init common |> updateWith PageHome HomeMsg model

                    ( Just Route.Roster, Just (PageRoster _) ) ->
                        ( model, Cmd.none )

                    ( Just Route.Roster, _ ) ->
                        Page.Roster.init common |> updateWith PageRoster RosterMsg model

                    ( Just (Route.Profile newEmail), Just (PageProfile profileModel) ) ->
                        -- Don't reload the page if the same member is already loaded
                        if
                            profileModel.member
                                |> remoteToMaybe
                                |> Maybe.map (\( member, _ ) -> member.email == newEmail)
                                |> Maybe.withDefault True
                        then
                            ( model, Cmd.none )

                        else
                            Page.Profile.init common newEmail |> updateWith PageProfile ProfileMsg model

                    ( Just (Route.Profile email), _ ) ->
                        Page.Profile.init common email |> updateWith PageProfile ProfileMsg model

                    ( Just Route.EditProfile, Just (PageEditProfile _) ) ->
                        ( model, Cmd.none )

                    ( Just Route.EditProfile, _ ) ->
                        Page.EditProfile.init common |> updateWith PageEditProfile EditProfileMsg model

                    ( Just (Route.Events _), Just (PageEvents _) ) ->
                        ( model, Cmd.none )

                    ( Just (Route.Events route), _ ) ->
                        Page.Events.init common route |> updateWith PageEvents EventsMsg model

                    ( Just (Route.EditCarpools _), Just (PageEditCarpools _) ) ->
                        ( model, Cmd.none )

                    ( Just (Route.EditCarpools route), _ ) ->
                        EditCarpools.init common route |> updateWith PageEditCarpools EditCarpoolsMsg model

                    ( Just (Route.Repertoire _), Just (PageRepertoire _) ) ->
                        ( model, Cmd.none )

                    ( Just (Route.Repertoire songId), _ ) ->
                        Page.Repertoire.init common songId |> updateWith PageRepertoire RepertoireMsg model

                    ( Just (Route.Minutes _), Just (PageMinutes _) ) ->
                        ( model, Cmd.none )

                    ( Just (Route.Minutes route), _ ) ->
                        Page.Minutes.init common route |> updateWith PageMinutes MinutesMsg model

                    ( Just (Route.Admin tab), Just (PageAdmin pageModel) ) ->
                        -- Only load the new tab if it isn't currently open
                        if tab |> Maybe.map (\t -> Page.Admin.tabIsActive pageModel t) |> Maybe.withDefault False then
                            ( model, Cmd.none )

                        else
                            Page.Admin.init common tab |> updateWith PageAdmin AdminMsg model

                    ( Just (Route.Admin tab), _ ) ->
                        Page.Admin.init common tab |> updateWith PageAdmin AdminMsg model

                    ( Nothing, _ ) ->
                        ( { model | page = Just <| PageNotFound common }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
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
            if not (url |> Url.toString |> String.startsWith "#/") then
                ( model, Nav.load (url |> Url.toString) )

            else
                let
                    route =
                        Route.fromUrl url

                    urlChange =
                        model.common
                            |> remoteToMaybe
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
        ( OnFetchCommon (Ok common), _ ) ->
            loadCurrentPage { model | common = Loaded common }

        ( OnFetchCommon (Err error), _ ) ->
            case model.route of
                Just Route.Login ->
                    ( { model | common = Failure error }, Cmd.none )

                Just Route.EditProfile ->
                    ( { model | common = Failure error }, Cmd.none )

                _ ->
                    ( model, Cmd.batch [ setToken Nothing, Nav.reload ] )

        ( OnUrlRequest request, _ ) ->
            urlRequest model request

        ( OnUrlChange url, _ ) ->
            loadCurrentPage { model | burgerOpened = False, route = Route.fromUrl url }

        ( ConfirmAccount, _ ) ->
            case ( model.common, model.confirmAccountModal ) of
                ( Loaded common, Nothing ) ->
                    ( { model | confirmAccountModal = Just <| ConfirmAccount.init common }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( CancelConfirmAccount, _ ) ->
            ( { model | confirmAccountModal = Nothing }, Cmd.none )

        ( ConfirmAccountMsg modalMsg, _ ) ->
            case model.confirmAccountModal of
                Just modalModel ->
                    let
                        ( newModel, newMsg ) =
                            ConfirmAccount.update modalModel modalMsg
                    in
                    ( { model | confirmAccountModal = Just newModel }, Cmd.map ConfirmAccountMsg newMsg )

                Nothing ->
                    ( model, Cmd.none )

        ( Tick time, _ ) ->
            ( { model | common = model.common |> mapLoaded (\c -> { c | now = time }) }, Cmd.none )

        ( ToggleBurger, _ ) ->
            ( { model | burgerOpened = not model.burgerOpened }, Cmd.none )

        ( IgnoreConfirmPrompt, _ ) ->
            ( { model | ignoredConfirmPrompt = True }, Cmd.none )

        ( HomeMsg pageMsg, Just (PageHome pageModel) ) ->
            Page.Home.update pageMsg pageModel |> updateWith PageHome HomeMsg model

        ( HomeMsg _, _ ) ->
            ( model, Cmd.none )

        ( LoginMsg pageMsg, Just (PageLogin pageModel) ) ->
            Page.Login.update pageMsg pageModel |> updateWith PageLogin LoginMsg model

        ( LoginMsg _, _ ) ->
            ( model, Cmd.none )

        ( RosterMsg pageMsg, Just (PageRoster pageModel) ) ->
            Page.Roster.update pageMsg pageModel |> updateWith PageRoster RosterMsg model

        ( RosterMsg _, _ ) ->
            ( model, Cmd.none )

        ( ProfileMsg pageMsg, Just (PageProfile pageModel) ) ->
            Page.Profile.update pageMsg pageModel |> updateWith PageProfile ProfileMsg model

        ( ProfileMsg _, _ ) ->
            ( model, Cmd.none )

        ( EditProfileMsg pageMsg, Just (PageEditProfile pageModel) ) ->
            Page.EditProfile.update pageMsg pageModel |> updateWith PageEditProfile EditProfileMsg model

        ( EditProfileMsg _, _ ) ->
            ( model, Cmd.none )

        ( EventsMsg pageMsg, Just (PageEvents pageModel) ) ->
            Page.Events.update pageMsg pageModel |> updateWith PageEvents EventsMsg model

        ( EventsMsg _, _ ) ->
            ( model, Cmd.none )

        ( EditCarpoolsMsg pageMsg, Just (PageEditCarpools pageModel) ) ->
            EditCarpools.update pageMsg pageModel |> updateWith PageEditCarpools EditCarpoolsMsg model

        ( EditCarpoolsMsg _, _ ) ->
            ( model, Cmd.none )

        ( RepertoireMsg pageMsg, Just (PageRepertoire pageModel) ) ->
            Page.Repertoire.update pageMsg pageModel |> updateWith PageRepertoire RepertoireMsg model

        ( RepertoireMsg _, _ ) ->
            ( model, Cmd.none )

        ( MinutesMsg pageMsg, Just (PageMinutes pageModel) ) ->
            Page.Minutes.update pageMsg pageModel |> updateWith PageMinutes MinutesMsg model

        ( MinutesMsg _, _ ) ->
            ( model, Cmd.none )

        ( ForgotPasswordMsg pageMsg, Just (PageForgotPassword pageModel) ) ->
            Page.ForgotPassword.update pageMsg pageModel |> updateWith PageForgotPassword ForgotPasswordMsg model

        ( ForgotPasswordMsg _, _ ) ->
            ( model, Cmd.none )

        ( ResetPasswordMsg pageMsg, Just (PageResetPassword pageModel) ) ->
            Page.ResetPassword.update pageMsg pageModel |> updateWith PageResetPassword ResetPasswordMsg model

        ( ResetPasswordMsg _, _ ) ->
            ( model, Cmd.none )

        ( AdminMsg pageMsg, Just (PageAdmin pageModel) ) ->
            Page.Admin.update pageMsg pageModel |> updateWith PageAdmin AdminMsg model

        ( AdminMsg _, _ ) ->
            ( model, Cmd.none )


updateWith : (pageModel -> Page) -> (pageMsg -> Msg) -> Model -> ( pageModel, Cmd pageMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( pageModel, subCmd ) =
    ( { model | page = Just <| toModel pageModel }
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
        maybeUser =
            model.common
                |> remoteToMaybe
                |> Maybe.andThen .user

        commonIsLoaded =
            isJust (model.common |> remoteToMaybe)

        showConfirmSemester =
            not model.ignoredConfirmPrompt
                && commonIsLoaded
                && (maybeUser
                        |> Maybe.map (\u -> isNothing u.enrollment)
                        |> Maybe.withDefault False
                   )

        confirmSemesterBanner =
            if showConfirmSemester then
                confirmAccountHeader
                    { ignoreConfirm = IgnoreConfirmPrompt
                    , confirmAccount = ConfirmAccount
                    }

            else
                text ""

        confirmAccountModal =
            case model.confirmAccountModal of
                Just modal ->
                    Basics.modal CancelConfirmAccount <|
                        (ConfirmAccount.view modal
                            |> Html.map ConfirmAccountMsg
                        )

                Nothing ->
                    text ""

        content =
            model.common
                |> Basics.remoteContent
                    (\_ ->
                        model.page
                            |> Maybe.map viewCurrentPage
                            |> Maybe.withDefault (text "")
                    )
    in
    { title = "GlubHub"
    , body =
        [ div [ id "app" ]
            [ navBar
                { common = model.common |> remoteToMaybe
                , burgerOpened = model.burgerOpened
                , toggleBurger = ToggleBurger
                }
            , confirmSemesterBanner
            , confirmAccountModal
            , div [ style "padding-bottom" "50px" ] []
            , div
                [ class "center"
                , style "height" "100%"
                ]
                [ content ]
            ]
        ]
    }


viewCurrentPage : Page -> Html Msg
viewCurrentPage page =
    case page of
        PageNotFound common ->
            Page.NotFound.notFound common

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

        PageEditCarpools pageModel ->
            EditCarpools.view pageModel |> Html.map EditCarpoolsMsg

        PageRepertoire pageModel ->
            Page.Repertoire.view pageModel |> Html.map RepertoireMsg

        PageMinutes pageModel ->
            Page.Minutes.view pageModel |> Html.map MinutesMsg

        PageForgotPassword pageModel ->
            Page.ForgotPassword.view pageModel |> Html.map ForgotPasswordMsg

        PageResetPassword pageModel ->
            Page.ResetPassword.view pageModel |> Html.map ResetPasswordMsg

        PageAdmin pageModel ->
            Page.Admin.view pageModel |> Html.map AdminMsg
