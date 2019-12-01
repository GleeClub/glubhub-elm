module Page.Admin exposing (Model, Msg(..), allTabs, currentTab, init, pageList, tabText, update, view)

import Components.Basics as Basics
import Components.SelectableList exposing (selectableList)
import Html exposing (Html, aside, div, li, section, td, text, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Maybe.Extra
import Models.Event exposing (FullEvent)
import Page.Admin.AbsenceRequests as AbsenceRequests
import Page.Admin.Announcements as Announcements
import Page.Admin.DocumentLinks as DocumentLinks
import Page.Admin.Dues as Dues
import Page.Admin.MakeAnnouncement as MakeAnnouncement
import Page.Admin.OfficerPositions as OfficerPositions
import Page.Admin.SitePermissions as SitePermissions
import Page.Admin.Uniforms as Uniforms
import Page.Events exposing (Msg)
import Route exposing (AdminTab(..))
import Utils exposing (Common, RemoteData(..))



---- MODEL ----


type alias Model =
    { common : Common
    , tab : Maybe FullAdminTab
    }


type FullAdminTab
    = FullAdminCreateEvent
    | FullAdminGigRequest
    | FullAdminMakeAnnouncement MakeAnnouncement.Model
    | FullAdminAbsenceRequests AbsenceRequests.Model
    | FullAdminEditSemester
    | FullAdminOfficerPositions OfficerPositions.Model
    | FullAdminAnnouncements Announcements.Model
    | FullAdminSitePermissions SitePermissions.Model
    | FullAdminUniforms Uniforms.Model
    | FullAdminDues Dues.Model
    | FullAdminDocumentLinks DocumentLinks.Model


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
    | MakeAnnouncementMsg MakeAnnouncement.Msg
    | AbsenceRequestsMsg AbsenceRequests.Msg
    | OfficerPositionsMsg OfficerPositions.Msg
    | AnnouncementsMsg Announcements.Msg
    | DuesMsg Dues.Msg
    | UniformsMsg Uniforms.Msg
    | SitePermissionsMsg SitePermissions.Msg
    | DocumentLinksMsg DocumentLinks.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.tab ) of
        ( SelectTab tab, _ ) ->
            changeTab model tab

        ( MakeAnnouncementMsg tabMsg, Just (FullAdminMakeAnnouncement tabModel) ) ->
            MakeAnnouncement.update tabMsg tabModel |> updateWith FullAdminMakeAnnouncement MakeAnnouncementMsg model

        ( MakeAnnouncementMsg tabMsg, _ ) ->
            ( model, Cmd.none )

        ( OfficerPositionsMsg tabMsg, Just (FullAdminOfficerPositions tabModel) ) ->
            OfficerPositions.update tabMsg tabModel |> updateWith FullAdminOfficerPositions OfficerPositionsMsg model

        ( OfficerPositionsMsg tabMsg, _ ) ->
            ( model, Cmd.none )

        ( AnnouncementsMsg tabMsg, Just (FullAdminAnnouncements tabModel) ) ->
            Announcements.update tabMsg tabModel |> updateWith FullAdminAnnouncements AnnouncementsMsg model

        ( AnnouncementsMsg tabMsg, _ ) ->
            ( model, Cmd.none )

        ( DuesMsg tabMsg, Just (FullAdminDues tabModel) ) ->
            Dues.update tabMsg tabModel |> updateWith FullAdminDues DuesMsg model

        ( DuesMsg tabMsg, _ ) ->
            ( model, Cmd.none )

        ( UniformsMsg tabMsg, Just (FullAdminUniforms tabModel) ) ->
            Uniforms.update tabMsg tabModel |> updateWith FullAdminUniforms UniformsMsg model

        ( UniformsMsg tabMsg, _ ) ->
            ( model, Cmd.none )

        ( AbsenceRequestsMsg tabMsg, Just (FullAdminAbsenceRequests tabModel) ) ->
            AbsenceRequests.update tabMsg tabModel |> updateWith FullAdminAbsenceRequests AbsenceRequestsMsg model

        ( AbsenceRequestsMsg tabMsg, _ ) ->
            ( model, Cmd.none )

        ( SitePermissionsMsg tabMsg, Just (FullAdminSitePermissions tabModel) ) ->
            SitePermissions.update tabMsg tabModel |> updateWith FullAdminSitePermissions SitePermissionsMsg model

        ( SitePermissionsMsg tabMsg, _ ) ->
            ( model, Cmd.none )

        ( DocumentLinksMsg tabMsg, Just (FullAdminDocumentLinks tabModel) ) ->
            DocumentLinks.update tabMsg tabModel |> updateWith FullAdminDocumentLinks DocumentLinksMsg model

        ( DocumentLinksMsg tabMsg, _ ) ->
            ( model, Cmd.none )


changeTab : Model -> AdminTab -> ( Model, Cmd Msg )
changeTab model tab =
    let
        ( newModel, newCmd ) =
            case tab of
                AdminCreateEvent ->
                    ( { model | tab = Just FullAdminCreateEvent }, Cmd.none )

                AdminGigRequest ->
                    ( { model | tab = Just FullAdminGigRequest }, Cmd.none )

                AdminMakeAnnouncement ->
                    MakeAnnouncement.init model.common |> updateWith FullAdminMakeAnnouncement MakeAnnouncementMsg model

                AdminAbsenceRequests ->
                    AbsenceRequests.init model.common |> updateWith FullAdminAbsenceRequests AbsenceRequestsMsg model

                AdminEditSemester ->
                    ( { model | tab = Just FullAdminEditSemester }, Cmd.none )

                AdminOfficerPositions ->
                    OfficerPositions.init model.common |> updateWith FullAdminOfficerPositions OfficerPositionsMsg model

                AdminAnnouncements ->
                    Announcements.init model.common |> updateWith FullAdminAnnouncements AnnouncementsMsg model

                AdminSitePermissions ->
                    SitePermissions.init model.common |> updateWith FullAdminSitePermissions SitePermissionsMsg model

                AdminUniforms ->
                    Uniforms.init model.common |> updateWith FullAdminUniforms UniformsMsg model

                AdminDues ->
                    Dues.init model.common |> updateWith FullAdminDues DuesMsg model

                AdminDocumentLinks ->
                    DocumentLinks.init model.common |> updateWith FullAdminDocumentLinks DocumentLinksMsg model
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


allTabs : List AdminTab
allTabs =
    [ AdminCreateEvent
    , AdminGigRequest
    , AdminMakeAnnouncement
    , AdminAnnouncements
    , AdminAbsenceRequests
    , AdminEditSemester
    , AdminOfficerPositions
    , AdminSitePermissions
    , AdminUniforms
    , AdminDues
    , AdminDocumentLinks
    ]


tabText : AdminTab -> String
tabText tab =
    case tab of
        AdminCreateEvent ->
            "Create Event"

        AdminGigRequest ->
            "Gig Requests"

        AdminMakeAnnouncement ->
            "Make Announcement"

        AdminAnnouncements ->
            "All Announcements"

        AdminAbsenceRequests ->
            "Absence Requests"

        AdminEditSemester ->
            "Edit Semester"

        AdminOfficerPositions ->
            "Officer Positions"

        AdminSitePermissions ->
            "Site Permissions"

        AdminUniforms ->
            "Uniforms"

        AdminDues ->
            "Dues"

        AdminDocumentLinks ->
            "Document Links"


tabIsActive : Model -> AdminTab -> Bool
tabIsActive model tab =
    case ( model.tab, tab ) of
        ( Just FullAdminCreateEvent, AdminCreateEvent ) ->
            True

        ( Just FullAdminGigRequest, AdminGigRequest ) ->
            True

        ( Just (FullAdminMakeAnnouncement _), AdminMakeAnnouncement ) ->
            True

        ( Just (FullAdminAbsenceRequests _), AdminAbsenceRequests ) ->
            True

        ( Just FullAdminEditSemester, AdminEditSemester ) ->
            True

        ( Just (FullAdminOfficerPositions _), AdminOfficerPositions ) ->
            True

        ( Just (FullAdminAnnouncements _), AdminAnnouncements ) ->
            True

        ( Just (FullAdminSitePermissions _), AdminSitePermissions ) ->
            True

        ( Just (FullAdminUniforms _), AdminUniforms ) ->
            True

        ( Just (FullAdminDues _), AdminDues ) ->
            True

        ( Just (FullAdminDocumentLinks _), AdminDocumentLinks ) ->
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
                    [ pageList model, currentTab model ]
                ]
            ]
        ]


pageList : Model -> Html Msg
pageList model =
    selectableList
        { listItems = Loaded allTabs
        , isSelected = tabIsActive model
        , messageIfEmpty = ""
        , onSelect = SelectTab
        , render = \tab -> [ td [] [ text <| tabText tab ] ]
        }


currentTab : Model -> Html Msg
currentTab model =
    case model.tab of
        Nothing ->
            Basics.box [ text "Select a menu item" ]

        Just tab ->
            tabContent model tab


tabContent : Model -> FullAdminTab -> Html Msg
tabContent model tab =
    case tab of
        FullAdminCreateEvent ->
            Basics.box [ text "Create Event" ]

        FullAdminGigRequest ->
            Basics.box [ text "Gig Requests" ]

        FullAdminMakeAnnouncement tabModel ->
            MakeAnnouncement.view tabModel |> Html.map MakeAnnouncementMsg

        FullAdminAbsenceRequests tabModel ->
            AbsenceRequests.view tabModel |> Html.map AbsenceRequestsMsg

        FullAdminEditSemester ->
            Basics.box [ text "Edit Semester" ]

        FullAdminOfficerPositions tabModel ->
            OfficerPositions.view tabModel |> Html.map OfficerPositionsMsg

        FullAdminAnnouncements tabModel ->
            Announcements.view tabModel |> Html.map AnnouncementsMsg

        FullAdminSitePermissions tabModel ->
            SitePermissions.view tabModel |> Html.map SitePermissionsMsg

        FullAdminUniforms tabModel ->
            Uniforms.view tabModel |> Html.map UniformsMsg

        FullAdminDues tabModel ->
            Dues.view tabModel |> Html.map DuesMsg

        FullAdminDocumentLinks tabModel ->
            DocumentLinks.view tabModel |> Html.map DocumentLinksMsg
