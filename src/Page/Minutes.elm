module Page.Minutes exposing (Model, Msg(..), editMinutes, init, loadAllMinutes, loadSingleMinutes, update, view, viewCompleteMinutes, viewSelectedMinutes, viewTab, viewTabBlock)

import Browser.Navigation as Nav
import Components.SelectableList exposing (SelectableListData, selectableList)
import Html exposing (Html, a, button, div, form, h1, img, input, label, section, span, text)
import Html.Attributes exposing (class, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (find)
import MD5
import Markdown exposing (Markdown)
import Maybe.Extra exposing (filter, isJust)
import Models.Document exposing (MeetingMinutes, meetingMinutesDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), getRequest, notFoundView, permittedTo, setToken, spinner)



---- MODEL ----


type alias Model =
    { common : Common
    , minutes : RemoteData (List MeetingMinutes)
    , selected : RemoteData MeetingMinutes
    , tab : Maybe MinutesTab
    }


init : Common -> Maybe Int -> ( Model, Cmd Msg )
init common selected =
    let
        ( tab, selectedMinutes, loadSingleMinutes ) =
            case selected of
                Just selectedId ->
                    ( Just Public, Loading, [ loadSingleMinutes common ] )

                Nothing ->
                    ( Nothing, NotAsked, [] )

        commands =
            [ loadAllMinutes common ] ++ loadSingleMinutes
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
    | UpdateUrl


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadAllMinutes (Ok minutes) ->
            ( { model | minutes = Loaded minutes }, Cmd.none )

        OnLoadAllMinutes (Err _) ->
            ( { model | minutes = Failure }, Cmd.none )

        OnLoadSingleMinutes (Ok minutes) ->
            ( { model | minutes = Loaded minutes }, UpdateUrl )

        OnLoadSingleMinutes (Err _) ->
            ( { model | minutes = Failure }, UpdateUrl )

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
            ( { model | tab = Just tab }, UpdateUrl )

        UpdateUrl ->
            let
                minutesId =
                    case model.selected of
                        Loaded minutes ->
                            Just minutes.id

                        _ ->
                            Nothing
            in
            ( model, Route.replaceUrl common.key Route.Minutes { id = minutesId, tab = model.tab } )



---- DATA ----


loadAllMinutes : Common -> Cmd Msg
loadAllMinutes common =
    getRequest common "/meeting_minutes" (Http.expectJson <| Decode.list meetingMinutesDecoder)


loadSingleMinutes : Common -> Int -> Cmd Msg
loadSingleMinutes common minutesId =
    let
        url =
            "/meeting_minutes/" ++ String.fromInt minutesId
    in
    getRequest common url (Http.expectJson meetingMinutesDecoder)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        isSelected minutes =
            case model.selected of
                Loaded selected ->
                    selected.id == minutes.id

                _ ->
                    False

        minutesList =
            selectableList
                { listItems = model.minutes
                , isSelected = isSelected
                , onSelect = \minutes -> SelectMinutes minutes.id
                , render = \minutes -> text minutes.name
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
        selectedMinutes =
            case ( model.minutes, model.selected ) of
                ( Loaded minutes, Just selectedId ) ->
                    minutes |> find (\m -> m.id == selectedId)

                ( _, _ ) ->
                    Nothing

        loaded =
            case model.minutes of
                Loading ->
                    False

                _ ->
                    True
    in
    case meetingMinutes of
        Nothing ->
            div [ class "box" ]
                [ p [] [ text "Select minutes" ] ]

        Just minutes ->
            div [ class "box" ]
                [ viewTabs, viewTabBlock minutes ]


viewTabs : Member -> Maybe MinutesTab -> Html Msg
viewTabs user selectedTab =
    let
        tabs =
            List.concat <|
                [ [ viewTab Public "Redacted" selectedTab ]
                , if user |> permittedTo viewCompleteMinutes then
                    [ viewTab Private "Complete" selectedTab ]

                  else
                    []
                , if user |> permittedTo editMinutes then
                    [ viewTab Edit "Edit" selectedTab ]

                  else
                    []
                ]
    in
    if (user |> permittedTo viewCompleteMinutes) or (user |> permittedTo editMinutes) then
        div [ class "tabs" ] [ ul [] tabs ]

    else
        div [ style "display" "none" ] []


viewTab : MinutesTab -> String -> Maybe MinutesTab -> Html Msg
viewTab tab name selectedTab =
    let
        isSelected =
            selectedTab |> filter (\t -> t == tab) |> isJust
    in
    li
        [ class <|
            if isSelected then
                "is-active"

            else
                ""
        , onClick <| SelectMinutes tab
        ]
        [ text name ]


viewTabBlock : MeetingMinutes -> MinutesTab -> Member -> Html Msg
viewTabBlock minutes tab user =
    case tab of
        Public ->
            Markdown.toHtml [] (minutes.public |> Maybe.withDefault "")

        Private ->
            if user |> permittedTo viewCompleteMinutes then
                Markdown.toHtml [] (minutes.public |> Maybe.withDefault "")

            else
                text "Slow down, cowboy! Who said you could see these here documents?"

        Edit ->
            if user |> permittedTo editMinutes then
                text "Edit minutes"

            else
                text "Slow down, cowboy! Who said you could edit these here documents?"
