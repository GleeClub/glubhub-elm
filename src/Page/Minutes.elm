module Page.Minutes exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Components.SelectableList exposing (selectableList)
import Error exposing (GreaseResult)
import Html exposing (Html, a, button, div, form, h1, img, input, label, li, p, section, span, td, text, ul)
import Html.Attributes exposing (class, href, id, placeholder, property, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (find)
import MD5
import Maybe.Extra exposing (filter, isJust)
import Models.Document exposing (MeetingMinutes, meetingMinutesDecoder)
import Models.Event exposing (Member)
import Route exposing (MinutesRoute, MinutesTab(..), Route)
import Task
import Utils exposing (Common, RemoteData(..), getRequest, permittedTo, rawHtml, remoteToMaybe, setToken)



---- MODEL ----


type alias Model =
    { common : Common
    , minutes : RemoteData (List MeetingMinutes)
    , selected : RemoteData MeetingMinutes
    , tab : Maybe MinutesTab
    }


init : Common -> MinutesRoute -> ( Model, Cmd Msg )
init common route =
    let
        ( tab, selectedMinutes, toLoadSingleMinutes ) =
            case route.id of
                Just selectedId ->
                    ( route.tab, Loading, [ loadSingleMinutes common selectedId ] )

                Nothing ->
                    ( Nothing, NotAsked, [] )

        commands =
            [ loadAllMinutes common ] ++ toLoadSingleMinutes
    in
    ( { common = common
      , minutes = Loading
      , selected = selectedMinutes
      , tab = tab
      }
    , Cmd.batch commands
    )


viewCompleteMinutes : String
viewCompleteMinutes =
    "view-complete-minutes"


editMinutes : String
editMinutes =
    "edit-minutes"



---- UPDATE ----


type Msg
    = OnLoadAllMinutes (GreaseResult (List MeetingMinutes))
    | OnLoadSingleMinutes (GreaseResult MeetingMinutes)
    | SelectMinutes Int
    | SelectTab MinutesTab


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadAllMinutes (Ok minutes) ->
            ( { model | minutes = Loaded (List.reverse minutes) }, Cmd.none )

        OnLoadAllMinutes (Err error) ->
            ( { model | minutes = Failure error }, Cmd.none )

        OnLoadSingleMinutes (Ok minutes) ->
            updateUrl { model | selected = Loaded minutes }

        OnLoadSingleMinutes (Err error) ->
            updateUrl { model | selected = Failure error }

        SelectMinutes selected ->
            let
                currentId =
                    case model.selected of
                        Loaded minutes ->
                            Just minutes.id

                        _ ->
                            Nothing
            in
            if currentId |> filter (\id -> id == selected) |> isJust then
                ( model, Cmd.none )

            else
                ( { model | selected = Loading }, loadSingleMinutes model.common selected )

        SelectTab tab ->
            updateUrl { model | tab = Just tab }



---- DATA ----


updateUrl : Model -> ( Model, Cmd Msg )
updateUrl model =
    ( model
    , Route.replaceUrl model.common.key <|
        Route.Minutes
            { id =
                model.selected
                    |> remoteToMaybe
                    |> Maybe.map .id
            , tab = model.tab
            }
    )


loadAllMinutes : Common -> Cmd Msg
loadAllMinutes common =
    getRequest common "/meeting_minutes" (Decode.list meetingMinutesDecoder)
        |> Task.attempt OnLoadAllMinutes


loadSingleMinutes : Common -> Int -> Cmd Msg
loadSingleMinutes common minutesId =
    let
        url =
            "/meeting_minutes/" ++ String.fromInt minutesId
    in
    getRequest common url meetingMinutesDecoder
        |> Task.attempt OnLoadSingleMinutes



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        isSelected minutes =
            model.selected
                |> remoteToMaybe
                |> Maybe.map (\s -> s.id == minutes.id)
                |> Maybe.withDefault False

        minutesList =
            selectableList
                { listItems = model.minutes
                , render = \minutes -> [ td [] [ text minutes.name ] ]
                , onSelect = \minutes -> SelectMinutes minutes.id
                , messageIfEmpty = "No minutes"
                , isSelected = isSelected
                }
    in
    div [ id "minutes" ]
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ minutesList
                    , div [ class "column" ]
                        [ viewSelectedMinutes model ]
                    ]
                ]
            ]
        ]


viewSelectedMinutes : Model -> Html Msg
viewSelectedMinutes model =
    let
        notSelected =
            p [] [ text "Select minutes" ]

        render minutes =
            div [] <|
                selectedMinutesTabBar model.common.user model.tab minutes.id
                    :: selectedMinutesTab minutes (model.tab |> Maybe.withDefault Public) model.common.user
    in
    Basics.box [ model.selected |> Basics.remoteContentFull notSelected render ]


selectedMinutesTabBar : Maybe Member -> Maybe MinutesTab -> Int -> Html Msg
selectedMinutesTabBar user selectedTab minutesId =
    let
        canViewCompleteMinutes =
            user |> Maybe.map (permittedTo viewCompleteMinutes) |> Maybe.withDefault False

        canEditMinutes =
            user |> Maybe.map (permittedTo editMinutes) |> Maybe.withDefault False

        tabs =
            List.concat <|
                [ [ singleTab Public selectedTab minutesId ]
                , if canViewCompleteMinutes then
                    [ singleTab Private selectedTab minutesId ]

                  else
                    []
                , if canEditMinutes then
                    [ singleTab Edit selectedTab minutesId ]

                  else
                    []
                ]
    in
    if canViewCompleteMinutes || canEditMinutes then
        div [ class "tabs" ] [ ul [] tabs ]

    else
        div [ style "display" "none" ] []


tabName : MinutesTab -> String
tabName tab =
    case tab of
        Public ->
            "Redacted"

        Private ->
            "Complete"

        Edit ->
            "Edit"


singleTab : MinutesTab -> Maybe MinutesTab -> Int -> Html Msg
singleTab tab selectedTab minutesId =
    let
        isSelected =
            selectedTab |> filter (\t -> t == tab) |> isJust
    in
    li []
        [ a
            [ onClick <| SelectTab tab
            , class <|
                if isSelected then
                    " is-active"

                else
                    ""
            ]
            [ text <| tabName tab ]
        ]


selectedMinutesTab : MeetingMinutes -> MinutesTab -> Maybe Member -> List (Html Msg)
selectedMinutesTab minutes tab user =
    case tab of
        Public ->
            minutes.public |> Maybe.withDefault "" |> rawHtml

        Private ->
            if user |> Maybe.map (permittedTo viewCompleteMinutes) |> Maybe.withDefault False then
                minutes.private |> Maybe.withDefault "" |> rawHtml

            else
                [ text "Slow down, cowboy! Who said you could see these here documents?" ]

        Edit ->
            if user |> Maybe.map (permittedTo editMinutes) |> Maybe.withDefault False then
                [ text "Edit minutes" ]

            else
                [ text "Slow down, cowboy! Who said you could edit these here documents?" ]
