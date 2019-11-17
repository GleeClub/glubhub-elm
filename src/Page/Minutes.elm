module Page.Minutes exposing (Model, Msg(..), editMinutes, init, loadAllMinutes, loadSingleMinutes, update, view, viewCompleteMinutes, viewSelectedMinutes, viewTab, viewTabBlock)

import Browser.Navigation as Nav
import Components.SelectableList exposing (SelectableListData, selectableList)
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
import Utils exposing (Common, RemoteData(..), getRequest, notFoundView, permittedTo, rawHtml, setToken, spinner)



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
    = OnLoadAllMinutes (Result Http.Error (List MeetingMinutes))
    | OnLoadSingleMinutes (Result Http.Error MeetingMinutes)
    | SelectMinutes Int
    | SelectTab MinutesTab


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadAllMinutes (Ok minutes) ->
            ( { model | minutes = Loaded (List.reverse minutes) }, Cmd.none )

        OnLoadAllMinutes (Err _) ->
            ( { model | minutes = Failure }, Cmd.none )

        OnLoadSingleMinutes (Ok minutes) ->
            updateUrl { model | selected = Loaded minutes }

        OnLoadSingleMinutes (Err _) ->
            updateUrl { model | selected = Failure }

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
    let
        minutesId =
            case model.selected of
                Loaded minutes ->
                    Just minutes.id

                _ ->
                    Nothing

        route =
            Route.Minutes { id = minutesId, tab = model.tab }
    in
    ( model, Route.replaceUrl model.common.key route )


loadAllMinutes : Common -> Cmd Msg
loadAllMinutes common =
    getRequest common "/meeting_minutes" (Http.expectJson OnLoadAllMinutes <| Decode.list meetingMinutesDecoder)


loadSingleMinutes : Common -> Int -> Cmd Msg
loadSingleMinutes common minutesId =
    let
        url =
            "/meeting_minutes/" ++ String.fromInt minutesId
    in
    getRequest common url (Http.expectJson OnLoadSingleMinutes meetingMinutesDecoder)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        isSelected : MeetingMinutes -> Bool
        isSelected minutes =
            case model.selected of
                Loaded selected ->
                    selected.id == minutes.id

                _ ->
                    False

        minutesList =
            selectableList
                { listItems = model.minutes
                , render = \minutes -> [ td [] [ text minutes.name ] ]
                , isSelected = isSelected
                , onSelect = \minutes -> SelectMinutes minutes.id
                , messageIfEmpty = "No minutes"
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
        content =
            case model.selected of
                NotAsked ->
                    [ p [] [ text "Select minutes" ] ]

                Loading ->
                    [ spinner ]

                Loaded minutes ->
                    [ viewTabs model.common.user model.tab minutes.id ]
                        ++ viewTabBlock minutes (model.tab |> Maybe.withDefault Public) model.common.user

                Failure ->
                    [ text "Whoops" ]
    in
    div [ class "box" ] content


viewTabs : Maybe Member -> Maybe MinutesTab -> Int -> Html Msg
viewTabs user selectedTab minutesId =
    let
        canViewCompleteMinutes =
            user |> Maybe.map (permittedTo viewCompleteMinutes) |> Maybe.withDefault False

        canEditMinutes =
            user |> Maybe.map (permittedTo editMinutes) |> Maybe.withDefault False

        tabs =
            List.concat <|
                [ [ viewTab Public selectedTab minutesId ]
                , if canViewCompleteMinutes then
                    [ viewTab Private selectedTab minutesId ]

                  else
                    []
                , if canEditMinutes then
                    [ viewTab Edit selectedTab minutesId ]

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


viewTab : MinutesTab -> Maybe MinutesTab -> Int -> Html Msg
viewTab tab selectedTab minutesId =
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


viewTabBlock : MeetingMinutes -> MinutesTab -> Maybe Member -> List (Html Msg)
viewTabBlock minutes tab user =
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
