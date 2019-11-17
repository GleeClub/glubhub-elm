module Page.Admin exposing (Model, Msg(..), allTabs, currentTab, init, pageList, tabText, update, view)

import Html exposing (Html, aside, div, li, section, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Maybe.Extra
import Models.Event exposing (FullEvent)
import Page.Events exposing (Msg)
import Route exposing (AdminTab(..))
import Utils exposing (Common)



---- MODEL ----


type alias Model =
    { common : Common
    , tab : Maybe AdminTab
    }


init : Common -> Maybe AdminTab -> ( Model, Cmd Msg )
init common tab =
    ( { common = common, tab = tab }, Cmd.none )



---- UPDATE ----


type Msg
    = SelectTab AdminTab


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            ( { model | tab = Just tab }, Route.replaceUrl model.common.key <| Route.Admin (Just tab) )



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



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ pageList, currentTab model.tab ]
                ]
            ]
        ]


pageList : Html Msg
pageList =
    let
        tabRowWithLink : AdminTab -> Html Msg
        tabRowWithLink tab =
            li [ onClick <| SelectTab tab ] [ text <| tabText tab ]
    in
    div [ class "column is-narrow" ]
        [ aside [ class "menu" ]
            [ div [ class "box" ]
                [ ul [ class "menu-list" ] (allTabs |> List.map tabRowWithLink) ]
            ]
        ]


currentTab : Maybe AdminTab -> Html Msg
currentTab maybeTab =
    let
        content =
            case maybeTab of
                Just tab ->
                    text <| tabText tab

                Nothing ->
                    text "Select a menu item"
    in
    div [ class "column" ]
        [ div [ class "box" ]
            [ div [] [ content ] ]
        ]
