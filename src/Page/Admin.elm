module Page.Admin exposing (Model, Msg(..), init, tabIsActive, tabText, update, view, visibleAdminTabs)

import Components.Basics as Basics
import Components.SelectableList exposing (selectableListWithDividers)
import Html exposing (Html, div, section, td, text)
import Html.Attributes exposing (class)
import Page.Admin.AbsenceRequests as AbsenceRequests
import Page.Admin.CreateEvent as CreateEvent
import Page.Admin.DocumentLinks as DocumentLinks
import Page.Admin.Dues as Dues
import Page.Admin.EditSemester as EditSemester
import Page.Admin.GigRequests as GigRequests
import Page.Admin.OfficerPositions as OfficerPositions
import Page.Admin.SitePermissions as SitePermissions
import Page.Admin.Uniforms as Uniforms
import Page.Admin.WebmasterTools as WebmasterTools
import Page.Events exposing (Msg)
import Permissions
import Route exposing (AdminTab(..))
import Utils exposing (Common, RemoteData(..))



---- MODEL ----


type alias Model =
    { common : Common
    , tab : Maybe FullAdminTab
    }


type FullAdminTab
    = FullAdminCreateEvent CreateEvent.Model
    | FullAdminGigRequests GigRequests.Model
    | FullAdminAbsenceRequests AbsenceRequests.Model
    | FullAdminEditSemester EditSemester.Model
    | FullAdminOfficerPositions OfficerPositions.Model
    | FullAdminSitePermissions SitePermissions.Model
    | FullAdminUniforms Uniforms.Model
    | FullAdminDues Dues.Model
    | FullAdminDocumentLinks DocumentLinks.Model
    | FullAdminWebmasterTools WebmasterTools.Model


init : Common -> Maybe AdminTab -> ( Model, Cmd Msg )
init common maybeTab =
    let
        model =
            { common = common, tab = Nothing }
    in
    case maybeTab of
        Nothing ->
            ( model, Cmd.none )

        Just tab ->
            changeTab model tab



---- UPDATE ----


type Msg
    = SelectTab AdminTab
    | AbsenceRequestsMsg AbsenceRequests.Msg
    | GigRequestsMsg GigRequests.Msg
    | OfficerPositionsMsg OfficerPositions.Msg
    | EditSemesterMsg EditSemester.Msg
    | DuesMsg Dues.Msg
    | UniformsMsg Uniforms.Msg
    | SitePermissionsMsg SitePermissions.Msg
    | DocumentLinksMsg DocumentLinks.Msg
    | WebmasterToolsMsg WebmasterTools.Msg
    | CreateEventMsg CreateEvent.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.tab ) of
        ( SelectTab tab, _ ) ->
            changeTab model tab

        ( OfficerPositionsMsg tabMsg, Just (FullAdminOfficerPositions tabModel) ) ->
            OfficerPositions.update tabMsg tabModel |> updateWith FullAdminOfficerPositions OfficerPositionsMsg model

        ( OfficerPositionsMsg _, _ ) ->
            ( model, Cmd.none )

        ( GigRequestsMsg tabMsg, Just (FullAdminGigRequests tabModel) ) ->
            GigRequests.update tabMsg tabModel |> updateWith FullAdminGigRequests GigRequestsMsg model

        ( GigRequestsMsg _, _ ) ->
            ( model, Cmd.none )

        ( DuesMsg tabMsg, Just (FullAdminDues tabModel) ) ->
            Dues.update tabMsg tabModel |> updateWith FullAdminDues DuesMsg model

        ( DuesMsg _, _ ) ->
            ( model, Cmd.none )

        ( UniformsMsg tabMsg, Just (FullAdminUniforms tabModel) ) ->
            Uniforms.update tabMsg tabModel |> updateWith FullAdminUniforms UniformsMsg model

        ( UniformsMsg _, _ ) ->
            ( model, Cmd.none )

        ( EditSemesterMsg tabMsg, Just (FullAdminEditSemester tabModel) ) ->
            EditSemester.update tabMsg tabModel |> updateWith FullAdminEditSemester EditSemesterMsg model

        ( EditSemesterMsg _, _ ) ->
            ( model, Cmd.none )

        ( CreateEventMsg tabMsg, Just (FullAdminCreateEvent tabModel) ) ->
            CreateEvent.update tabMsg tabModel |> updateWith FullAdminCreateEvent CreateEventMsg model

        ( CreateEventMsg _, _ ) ->
            ( model, Cmd.none )

        ( AbsenceRequestsMsg tabMsg, Just (FullAdminAbsenceRequests tabModel) ) ->
            AbsenceRequests.update tabMsg tabModel |> updateWith FullAdminAbsenceRequests AbsenceRequestsMsg model

        ( AbsenceRequestsMsg _, _ ) ->
            ( model, Cmd.none )

        ( SitePermissionsMsg tabMsg, Just (FullAdminSitePermissions tabModel) ) ->
            SitePermissions.update tabMsg tabModel |> updateWith FullAdminSitePermissions SitePermissionsMsg model

        ( SitePermissionsMsg _, _ ) ->
            ( model, Cmd.none )

        ( DocumentLinksMsg tabMsg, Just (FullAdminDocumentLinks tabModel) ) ->
            DocumentLinks.update tabMsg tabModel |> updateWith FullAdminDocumentLinks DocumentLinksMsg model

        ( DocumentLinksMsg _, _ ) ->
            ( model, Cmd.none )

        ( WebmasterToolsMsg tabMsg, Just (FullAdminWebmasterTools tabModel) ) ->
            WebmasterTools.update tabMsg tabModel |> updateWith FullAdminWebmasterTools WebmasterToolsMsg model

        ( WebmasterToolsMsg _, _ ) ->
            ( model, Cmd.none )


changeTab : Model -> AdminTab -> ( Model, Cmd Msg )
changeTab model tab =
    let
        ( newModel, newCmd ) =
            case tab of
                AdminCreateEvent gigRequestId ->
                    CreateEvent.init model.common gigRequestId |> updateWith FullAdminCreateEvent CreateEventMsg model

                AdminGigRequest ->
                    GigRequests.init model.common |> updateWith FullAdminGigRequests GigRequestsMsg model

                AdminAbsenceRequests ->
                    AbsenceRequests.init model.common |> updateWith FullAdminAbsenceRequests AbsenceRequestsMsg model

                AdminEditSemester ->
                    EditSemester.init model.common |> updateWith FullAdminEditSemester EditSemesterMsg model

                AdminOfficerPositions ->
                    OfficerPositions.init model.common |> updateWith FullAdminOfficerPositions OfficerPositionsMsg model

                AdminSitePermissions ->
                    SitePermissions.init model.common |> updateWith FullAdminSitePermissions SitePermissionsMsg model

                AdminUniforms ->
                    Uniforms.init model.common |> updateWith FullAdminUniforms UniformsMsg model

                AdminDues ->
                    Dues.init model.common |> updateWith FullAdminDues DuesMsg model

                AdminDocumentLinks ->
                    DocumentLinks.init model.common |> updateWith FullAdminDocumentLinks DocumentLinksMsg model

                AdminWebmasterTools ->
                    WebmasterTools.init model.common |> updateWith FullAdminWebmasterTools WebmasterToolsMsg model
    in
    ( newModel
    , Cmd.batch
        [ newCmd
        , Route.replaceUrl model.common.key <| Route.Admin (Just tab)
        ]
    )


updateWith :
    (tabModel -> FullAdminTab)
    -> (tabMsg -> Msg)
    -> Model
    -> ( tabModel, Cmd tabMsg )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( tabModel, subCmd ) =
    ( { model | tab = Just (toModel tabModel) }
    , Cmd.map toMsg subCmd
    )



---- DATA ----


visibleAdminTabs : Common -> List (List AdminTab)
visibleAdminTabs common =
    let
        permittedTo permission =
            common.user
                |> Maybe.map (Utils.permittedTo permission)
                |> Maybe.withDefault False

        isWebmaster =
            common.user
                |> Maybe.map (\u -> u.positions |> List.any ((==) "Webmaster"))
                |> Maybe.withDefault False

        allAdminTabs =
            [ [ ( AdminCreateEvent Nothing, permittedTo Permissions.createEvent )
              , ( AdminGigRequest, permittedTo Permissions.processGigRequests )
              , ( AdminAbsenceRequests, permittedTo Permissions.processAbsenceRequests )
              ]
            , [ ( AdminEditSemester, permittedTo Permissions.editSemester )
              , ( AdminDocumentLinks, permittedTo Permissions.editLinks )
              , ( AdminDues, permittedTo Permissions.editTransaction )
              , ( AdminOfficerPositions, permittedTo Permissions.editOfficers )
              , ( AdminUniforms, permittedTo Permissions.editUniforms )
              , ( AdminSitePermissions, permittedTo Permissions.editPermissions )
              ]
            , [ ( AdminWebmasterTools, isWebmaster )
              ]
            ]
    in
    allAdminTabs
        |> List.map
            (List.filterMap
                (\( tab, visible ) ->
                    if visible then
                        Just tab

                    else
                        Nothing
                )
            )
        |> List.filter (not << List.isEmpty)


tabText : AdminTab -> String
tabText tab =
    case tab of
        AdminCreateEvent _ ->
            "Create Event"

        AdminGigRequest ->
            "Gig Requests"

        AdminAbsenceRequests ->
            "Absence Requests"

        AdminEditSemester ->
            "Edit the Semester"

        AdminOfficerPositions ->
            "Edit Officers"

        AdminSitePermissions ->
            "Edit Permissions"

        AdminUniforms ->
            "Uniforms"

        AdminDues ->
            "Money"

        AdminDocumentLinks ->
            "Edit Documents"

        AdminWebmasterTools ->
            "Upload API or Site"


tabIsActive : Model -> AdminTab -> Bool
tabIsActive model tab =
    case ( model.tab, tab ) of
        ( Just (FullAdminCreateEvent _), AdminCreateEvent _ ) ->
            True

        ( Just (FullAdminGigRequests _), AdminGigRequest ) ->
            True

        ( Just (FullAdminAbsenceRequests _), AdminAbsenceRequests ) ->
            True

        ( Just (FullAdminEditSemester _), AdminEditSemester ) ->
            True

        ( Just (FullAdminOfficerPositions _), AdminOfficerPositions ) ->
            True

        ( Just (FullAdminSitePermissions _), AdminSitePermissions ) ->
            True

        ( Just (FullAdminUniforms _), AdminUniforms ) ->
            True

        ( Just (FullAdminDues _), AdminDues ) ->
            True

        ( Just (FullAdminDocumentLinks _), AdminDocumentLinks ) ->
            True

        ( Just (FullAdminWebmasterTools _), AdminWebmasterTools ) ->
            True

        ( _, _ ) ->
            False



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ pageList model
                    , model.tab
                        |> Maybe.map tabContent
                        |> Maybe.withDefault
                            (Basics.narrowColumn
                                [ Basics.box [ text "Please select a menu item." ] ]
                            )
                    ]
                ]
            ]
        ]


pageList : Model -> Html Msg
pageList model =
    selectableListWithDividers
        { listItems = Loaded (visibleAdminTabs model.common)
        , isSelected = tabIsActive model
        , messageIfEmpty = ""
        , onSelect = SelectTab
        , render = \tab -> [ td [] [ text <| tabText tab ] ]
        , contentAtTop = text ""
        , contentAtBottom = text ""
        }


tabContent : FullAdminTab -> Html Msg
tabContent tab =
    case tab of
        FullAdminCreateEvent tabModel ->
            CreateEvent.view tabModel |> Html.map CreateEventMsg

        FullAdminGigRequests tabModel ->
            GigRequests.view tabModel |> Html.map GigRequestsMsg

        FullAdminAbsenceRequests tabModel ->
            AbsenceRequests.view tabModel |> Html.map AbsenceRequestsMsg

        FullAdminEditSemester tabModel ->
            EditSemester.view tabModel |> Html.map EditSemesterMsg

        FullAdminOfficerPositions tabModel ->
            OfficerPositions.view tabModel |> Html.map OfficerPositionsMsg

        FullAdminSitePermissions tabModel ->
            SitePermissions.view tabModel |> Html.map SitePermissionsMsg

        FullAdminUniforms tabModel ->
            Uniforms.view tabModel |> Html.map UniformsMsg

        FullAdminDues tabModel ->
            Dues.view tabModel |> Html.map DuesMsg

        FullAdminDocumentLinks tabModel ->
            DocumentLinks.view tabModel |> Html.map DocumentLinksMsg

        FullAdminWebmasterTools tabModel ->
            WebmasterTools.view tabModel |> Html.map WebmasterToolsMsg
