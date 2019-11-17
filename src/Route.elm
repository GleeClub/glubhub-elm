module Route exposing (AdminTab(..), EventRoute, EventTab(..), MinutesRoute, MinutesTab(..), Route(..), adminTabString, eventTabString, fromUrl, href, loadPage, minutesTabString, parser, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, custom, int, oneOf, s, string)



-- ROUTING


type Route
    = Home
    | Login
    | Roster
    | Profile String
    | EditProfile
    | Events EventRoute
    | Repertoire (Maybe Int)
    | Minutes MinutesRoute
    | ForgotPassword
    | Admin (Maybe AdminTab)


type alias EventRoute =
    { id : Maybe Int
    , tab : Maybe EventTab
    }


type alias MinutesRoute =
    { id : Maybe Int
    , tab : Maybe MinutesTab
    }


type EventTab
    = EventDetails
    | EventAttendees
    | EventSetlist
    | EventCarpools
    | EventRequestAbsence


eventTabString : EventTab -> String
eventTabString tab =
    case tab of
        EventDetails ->
            "details"

        EventAttendees ->
            "attendees"

        EventSetlist ->
            "setlist"

        EventCarpools ->
            "carpools"

        EventRequestAbsence ->
            "request-absence"


eventTabParser : String -> Maybe EventTab
eventTabParser tab =
    case tab of
        "details" ->
            Just EventDetails

        "attendees" ->
            Just EventAttendees

        "setlist" ->
            Just EventSetlist

        "carpools" ->
            Just EventCarpools

        "request-absence" ->
            Just EventRequestAbsence

        _ ->
            Nothing


type MinutesTab
    = Public
    | Private
    | Edit


minutesTabString : MinutesTab -> String
minutesTabString tab =
    case tab of
        Public ->
            "public"

        Private ->
            "private"

        Edit ->
            "edit"


minutesTabParser : String -> Maybe MinutesTab
minutesTabParser tab =
    case tab of
        "public" ->
            Just Public

        "private" ->
            Just Private

        "edit" ->
            Just Edit

        _ ->
            Nothing


type AdminTab
    = AdminCreateEvent
    | AdminGigRequest
    | AdminMakeAnnouncement
    | AdminAbsenceRequests
    | AdminEditSemester
    | AdminOfficerPositions
    | AdminAnnouncements
    | AdminSitePermissions
    | AdminUniforms
    | AdminDues
    | AdminDocumentLinks


adminTabString : AdminTab -> String
adminTabString tab =
    case tab of
        AdminCreateEvent ->
            "create-event"

        AdminGigRequest ->
            "gig-request"

        AdminMakeAnnouncement ->
            "make-announcement"

        AdminAbsenceRequests ->
            "absence-requests"

        AdminEditSemester ->
            "edit-semester"

        AdminOfficerPositions ->
            "officer-positions"

        AdminAnnouncements ->
            "announcements"

        AdminSitePermissions ->
            "site-permissions"

        AdminUniforms ->
            "uniforms"

        AdminDues ->
            "dues"

        AdminDocumentLinks ->
            "document-links"


adminTabParser : String -> Maybe AdminTab
adminTabParser tab =
    case tab of
        "create-event" ->
            Just AdminCreateEvent

        "gig-request" ->
            Just AdminGigRequest

        "make-announcement" ->
            Just AdminMakeAnnouncement

        "absence-requests" ->
            Just AdminAbsenceRequests

        "edit-semester" ->
            Just AdminEditSemester

        "officer-positions" ->
            Just AdminOfficerPositions

        "announcements" ->
            Just AdminAnnouncements

        "site-permissions" ->
            Just AdminSitePermissions

        "uniforms" ->
            Just AdminUniforms

        "dues" ->
            Just AdminDues

        "document-links" ->
            Just AdminDocumentLinks

        _ ->
            Nothing


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Roster (s "roster")
        , Parser.map Profile (s "profile" </> string)
        , Parser.map EditProfile (s "profile")

        -- /events, /events/{id}, and /events/{id}/{tab}
        , Parser.map (Events { id = Nothing, tab = Nothing }) (s "events")
        , Parser.map (\id -> Events { id = Just id, tab = Nothing }) (s "events" </> int)
        , Parser.map (\id tab -> Events { id = Just id, tab = Just tab }) (s "events" </> int </> custom "eventTab" eventTabParser)

        -- /repertoire and /repertoire/{id}
        , Parser.map (Repertoire Nothing) (s "repertoire")
        , Parser.map (\id -> Repertoire (Just id)) (s "repertoire" </> int)

        -- /minutes, /minutes/{id}, and /minutes/{id}/{tab}
        , Parser.map (Minutes { id = Nothing, tab = Nothing }) (s "minutes")
        , Parser.map (\id -> Minutes { id = Just id, tab = Nothing }) (s "minutes" </> int)
        , Parser.map (\id tab -> Minutes { id = Just id, tab = Just tab }) (s "minutes" </> int </> custom "minutesTab" minutesTabParser)
        , Parser.map ForgotPassword (s "forgot-password")

        -- /admin and /admin/{tab}
        , Parser.map (Admin Nothing) (s "admin")
        , Parser.map (\tab -> Admin (Just tab)) (s "admin" </> custom "adminTab" adminTabParser)
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


loadPage : Route -> Cmd msg
loadPage route =
    Nav.load (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = url.fragment |> Maybe.withDefault "", fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Roster ->
                    [ "roster" ]

                Profile email ->
                    [ "profile", email ]

                EditProfile ->
                    [ "profile" ]

                Events eventRoute ->
                    case ( eventRoute.id, eventRoute.tab ) of
                        ( Nothing, _ ) ->
                            [ "events" ]

                        ( Just id, Nothing ) ->
                            [ "events", String.fromInt id ]

                        ( Just id, Just tab ) ->
                            [ "events", String.fromInt id, eventTabString tab ]

                Repertoire songId ->
                    case songId of
                        Nothing ->
                            [ "repertoire" ]

                        Just id ->
                            [ "repertoire", String.fromInt id ]

                Minutes minutesRoute ->
                    case ( minutesRoute.id, minutesRoute.tab ) of
                        ( Nothing, _ ) ->
                            [ "minutes" ]

                        ( Just id, Nothing ) ->
                            [ "minutes", String.fromInt id ]

                        ( Just id, Just tab ) ->
                            [ "minutes", String.fromInt id, minutesTabString tab ]

                ForgotPassword ->
                    [ "forgot-password" ]

                Admin adminTab ->
                    case adminTab of
                        Nothing ->
                            [ "admin" ]

                        Just tab ->
                            [ "admin", adminTabString tab ]
    in
    "#/" ++ String.join "/" pieces
