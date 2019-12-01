module Page.Admin.Announcements exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, form, h1, i, img, input, label, li, option, p, section, select, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, href, id, src, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import List.Extra exposing (find)
import Models.Document exposing (Announcement, announcementDecoder)
import Models.Event exposing (Member, MemberRole, memberDecoder, memberRoleDecoder)
import Models.Info exposing (Role)
import Route exposing (AdminTab(..), Route)
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), getRequest, mapLoaded, postRequest, resultToRemote, resultToSubmissionState)



---- MODEL ----


type alias Model =
    { common : Common
    , announcements : RemoteData (List Announcement)
    , viewArchived : Bool
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , announcements = Loading
      , viewArchived = False
      }
    , loadAnnouncements common
    )



---- UPDATE ----


type Msg
    = OnLoadAnnouncements (GreaseResult (List Announcement))
    | ToggleViewArchived


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadAnnouncements announcements ->
            ( { model | announcements = resultToRemote announcements }, Cmd.none )

        ToggleViewArchived ->
            ( { model | viewArchived = not model.viewArchived }, Cmd.none )



---- DATA ----


loadAnnouncements : Common -> Cmd Msg
loadAnnouncements common =
    getRequest common "/announcements?all=true" (Decode.list announcementDecoder)
        |> Task.map (List.sortBy (\a -> posixToMillis a.time))
        |> Task.attempt OnLoadAnnouncements



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Announcements"
        , Basics.box
            [ model.announcements
                |> Basics.remoteContent
                    (\announcements ->
                        div []
                            (span [ style "float" "right" ]
                                [ viewArchivedCheckbox model.viewArchived ]
                                :: (announcements
                                        |> List.filter (\a -> model.viewArchived || not a.archived)
                                        |> List.map singleAnnouncement
                                   )
                            )
                    )
            ]
        ]


viewArchivedCheckbox : Bool -> Html Msg
viewArchivedCheckbox viewArchived =
    label [ class "checkbox" ]
        [ input [ type_ "checkbox", onCheck (\_ -> ToggleViewArchived) ] []
        , text "View Archived Announcements"
        ]


singleAnnouncement : Announcement -> Html Msg
singleAnnouncement announcement =
    Basics.box
        [ text announcement.content ]
