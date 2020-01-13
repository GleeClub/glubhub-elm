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
    | EditCarpools Int
    | Repertoire (Maybe Int)
    | Minutes MinutesRoute
    | ForgotPassword
    | ResetPassword (Maybe String)
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
    | EventAttendance
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

        EventAttendance ->
            "attendance"

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

        "attendance" ->
            Just EventAttendance

        "setlist" ->
            Just EventSetlist

        "carpools" ->
            Just EventCarpools

        "request-absence" ->
            Just EventRequestAbsence

        _ ->
            Nothing


type MinutesTab
    = PublicMinutes
    | PrivateMinutes
    | EditMinutes


minutesTabString : MinutesTab -> String
minutesTabString tab =
    case tab of
        PublicMinutes ->
            "public"

        PrivateMinutes ->
            "private"

        EditMinutes ->
            "edit"


minutesTabParser : String -> Maybe MinutesTab
minutesTabParser tab =
    case tab of
        "public" ->
            Just PublicMinutes

        "private" ->
            Just PrivateMinutes

        "edit" ->
            Just EditMinutes

        _ ->
            Nothing


type AdminTab
    = AdminCreateEvent (Maybe Int)
    | AdminGigRequest
    | AdminAbsenceRequests
    | AdminEditSemester
    | AdminOfficerPositions
    | AdminSitePermissions
    | AdminUniforms
    | AdminDues
    | AdminDocumentLinks
    | AdminWebmasterTools


adminTabString : AdminTab -> String
adminTabString tab =
    case tab of
        AdminCreateEvent Nothing ->
            "create-event"

        AdminCreateEvent (Just gigRequestId) ->
            "create-event/" ++ String.fromInt gigRequestId

        AdminGigRequest ->
            "gig-request"

        AdminAbsenceRequests ->
            "absence-requests"

        AdminEditSemester ->
            "edit-semester"

        AdminOfficerPositions ->
            "officer-positions"

        AdminSitePermissions ->
            "site-permissions"

        AdminUniforms ->
            "uniforms"

        AdminDues ->
            "dues"

        AdminDocumentLinks ->
            "document-links"

        AdminWebmasterTools ->
            "webmaster-tools"


adminTabParser : String -> Maybe AdminTab
adminTabParser tab =
    case tab of
        "create-event" ->
            Just (AdminCreateEvent Nothing)

        "gig-request" ->
            Just AdminGigRequest

        "absence-requests" ->
            Just AdminAbsenceRequests

        "edit-semester" ->
            Just AdminEditSemester

        "officer-positions" ->
            Just AdminOfficerPositions

        "site-permissions" ->
            Just AdminSitePermissions

        "uniforms" ->
            Just AdminUniforms

        "dues" ->
            Just AdminDues

        "document-links" ->
            Just AdminDocumentLinks

        "webmaster-tools" ->
            Just AdminWebmasterTools

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
        , Parser.map EditCarpools (s "events" </> int </> s "edit-carpools")

        -- /repertoire and /repertoire/{id}
        , Parser.map (Repertoire Nothing) (s "repertoire")
        , Parser.map (\id -> Repertoire <| Just id) (s "repertoire" </> int)

        -- /minutes, /minutes/{id}, and /minutes/{id}/{tab}
        , Parser.map (Minutes { id = Nothing, tab = Nothing }) (s "minutes")
        , Parser.map (\id -> Minutes { id = Just id, tab = Nothing }) (s "minutes" </> int)
        , Parser.map (\id tab -> Minutes { id = Just id, tab = Just tab }) (s "minutes" </> int </> custom "minutesTab" minutesTabParser)

        -- /forgot-password, /reset-password, and /reset-password/{token}
        , Parser.map ForgotPassword (s "forgot-password")
        , Parser.map (ResetPassword Nothing) (s "reset-password")
        , Parser.map (\token -> ResetPassword (Just token)) (s "reset-password" </> string)

        -- /admin and /admin/{tab}
        , Parser.map (Admin Nothing) (s "admin")
        , Parser.map (\tab -> Admin (Just tab)) (s "admin" </> custom "adminTab" adminTabParser)
        , Parser.map (\gigRequestId -> Admin (Just <| AdminCreateEvent (Just gigRequestId))) (s "admin" </> s "create-event" </> int)
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

                EditCarpools eventId ->
                    [ "events", String.fromInt eventId, "edit-carpools" ]

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

                ResetPassword Nothing ->
                    [ "reset-password" ]

                ResetPassword (Just token) ->
                    [ "reset-password", token ]

                Admin adminTab ->
                    case adminTab of
                        Nothing ->
                            [ "admin" ]

                        Just tab ->
                            [ "admin", adminTabString tab ]
    in
    "#/" ++ String.join "/" pieces
