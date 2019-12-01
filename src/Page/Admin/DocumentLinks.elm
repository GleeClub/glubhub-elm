module Page.Admin.DocumentLinks exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, form, h1, i, img, input, label, section, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, colspan, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import List.Extra exposing (groupWhile, removeAt, setAt, updateAt)
import Models.Document exposing (DocumentLink, documentLinkDecoder)
import Models.Event exposing (EventAttendee, Member, eventAttendeeDecoder)
import Route exposing (Route)
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), alert, deleteRequest, getRequest, mapLoaded, postRequest, resultToRemote, resultToSubmissionState)



---- MODEL ----


type alias Model =
    { common : Common
    , links : RemoteData (List DocumentLink)
    , newLink : DocumentLink
    , state : SubmissionState
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , links = Loading
      , newLink = emptyDocumentLink
      , state = NotSentYet
      }
    , loadDocumentLinks common
    )


emptyDocumentLink : DocumentLink
emptyDocumentLink =
    { name = "", url = "" }


editLinksPermission : String
editLinksPermission =
    "edit-links"



---- UPDATE ----


type Msg
    = OnLoadLinks (GreaseResult (List DocumentLink))
    | OnChangeLink (GreaseResult ())
    | UpdateLink DocumentLink Int
    | SendLinkUpdate Int
    | DeleteLink Int
    | InputNewName String
    | InputNewUrl String
    | CreateNewLink


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadLinks linksResult ->
            ( { model | links = resultToRemote linksResult }, Cmd.none )

        OnChangeLink result ->
            ( { model | state = resultToSubmissionState result }, Cmd.none )

        UpdateLink link linkIndex ->
            ( { model | links = model.links |> mapLoaded (setAt linkIndex link) }, Cmd.none )

        SendLinkUpdate index ->
            case model |> findLink index of
                Just link ->
                    ( { model | state = Sending }, updateDocumentLink model.common link )

                Nothing ->
                    ( model, Cmd.none )

        DeleteLink index ->
            case ( model |> findLink index, model.links ) of
                ( Just link, Loaded links ) ->
                    ( { model
                        | links = Loaded (links |> removeAt index)
                        , state = Sending
                      }
                    , deleteDocumentLink model.common link
                    )

                _ ->
                    ( model, Cmd.none )

        InputNewName name ->
            ( { model | newLink = { name = name, url = model.newLink.url } }, Cmd.none )

        InputNewUrl url ->
            ( { model | newLink = { name = model.newLink.name, url = url } }, Cmd.none )

        CreateNewLink ->
            ( { model
                | newLink = emptyDocumentLink
                , links = model.links |> mapLoaded (\links -> links ++ [ model.newLink ])
                , state = Sending
              }
            , newDocumentLink model.common model.newLink
            )



---- DATA ----


findLink : Int -> Model -> Maybe DocumentLink
findLink index model =
    case model.links of
        Loaded links ->
            links |> List.Extra.getAt index

        _ ->
            Nothing


loadDocumentLinks : Common -> Cmd Msg
loadDocumentLinks common =
    getRequest common "/google_docs" (Decode.list documentLinkDecoder)
        |> Task.attempt OnLoadLinks


updateDocumentLink : Common -> DocumentLink -> Cmd Msg
updateDocumentLink common link =
    let
        url =
            "/google_docs/" ++ link.name

        body =
            link |> serializeDocumentLink
    in
    postRequest common url body
        |> Task.attempt OnChangeLink


deleteDocumentLink : Common -> DocumentLink -> Cmd Msg
deleteDocumentLink common link =
    deleteRequest common ("/google_docs/" ++ link.name)
        |> Task.attempt OnChangeLink


newDocumentLink : Common -> DocumentLink -> Cmd Msg
newDocumentLink common link =
    postRequest common "/google_docs" (serializeDocumentLink link)
        |> Task.attempt OnChangeLink


serializeDocumentLink : DocumentLink -> Encode.Value
serializeDocumentLink link =
    Encode.object
        [ ( "name", Encode.string link.name )
        , ( "url", Encode.string link.url )
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Document Links"
        , Basics.box
            [ model.links |> Basics.remoteContent (documentLinkTable model.newLink)
            , Basics.submissionStateBox model.state
            ]
        ]


documentLinkTable : DocumentLink -> List DocumentLink -> Html Msg
documentLinkTable newLink allLinks =
    let
        linkRows =
            allLinks |> List.indexedMap linkRow

        newHeaderRow =
            tr [] [ td [] [ b [] [ text "New" ] ] ]
    in
    table [ style "border-spacing" "5px", style "border-collapse" "separate" ]
        (linkRows ++ [ newHeaderRow, newLinkRow newLink ])


linkRow : Int -> DocumentLink -> Html Msg
linkRow index link =
    tr []
        [ td [ style "padding-right" "10px" ]
            [ span [ style "display" "inline-block", style "vertical-align" "middle" ]
                [ text <| link.name ]
            ]
        , td []
            [ input
                [ type_ "text"
                , class "input"
                , value link.url
                , placeholder "URL"
                , onInput (\url -> UpdateLink { link | url = url } index)
                , onBlur (SendLinkUpdate index)
                ]
                []
            ]
        , td []
            [ span [ style "display" "inline-block", style "vertical-align" "middle" ]
                [ button
                    [ class "delete"
                    , attribute "aria-label" "delete"
                    , onClick (DeleteLink index)
                    ]
                    []
                ]
            ]
        ]


newLinkRow : DocumentLink -> Html Msg
newLinkRow newLink =
    tr []
        [ td []
            [ input
                [ type_ "text"
                , class "input"
                , value newLink.name
                , placeholder "Name"
                , onInput InputNewName
                ]
                []
            ]
        , td []
            [ input
                [ type_ "text"
                , class "input"
                , value newLink.url
                , placeholder "URL"
                , onInput InputNewUrl
                ]
                []
            ]
        , td []
            [ button [ class "button", onClick CreateNewLink ]
                [ text "sí" ]
            ]
        ]
