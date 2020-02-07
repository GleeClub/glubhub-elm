module Page.Admin.DocumentLinks exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.Forms as Forms exposing (textInput)
import Error exposing (GreaseResult)
import Html exposing (Html, b, button, div, span, table, td, text, tr)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (string)
import Json.Encode as Encode
import List.Extra exposing (removeAt, setAt)
import Models.Info exposing (DocumentLink, documentLinkDecoder)
import Task
import Utils
    exposing
        ( Common
        , RemoteData(..)
        , SubmissionState(..)
        , deleteRequest
        , getRequest
        , mapLoaded
        , postRequest
        , remoteToMaybe
        , resultToRemote
        , resultToSubmissionState
        )



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



---- UPDATE ----


type Msg
    = OnLoadLinks (GreaseResult (List DocumentLink))
    | OnChangeLink (GreaseResult ())
    | UpdateLink DocumentLink Int
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
            ( { model
                | links = model.links |> mapLoaded (setAt linkIndex link)
                , state = Sending
              }
            , updateDocumentLink model.common link
            )

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
    model.links
        |> remoteToMaybe
        |> Maybe.andThen (List.Extra.getAt index)


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
            tr []
                [ td []
                    [ b [] [ text "New" ] ]
                ]
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
            [ textInput Forms.string
                { value = link.url
                , onInput = \url -> UpdateLink { link | url = url } index
                , attrs = [ Forms.Placeholder "URL" ]
                }
            ]
        , td []
            [ span [ style "display" "inline-block", style "vertical-align" "middle" ]
                [ Buttons.delete (DeleteLink index) ]
            ]
        ]


newLinkRow : DocumentLink -> Html Msg
newLinkRow newLink =
    tr []
        [ td []
            [ textInput Forms.string
                { value = newLink.name
                , onInput = InputNewName
                , attrs = [ Forms.Placeholder "Name" ]
                }
            ]
        , td []
            [ textInput Forms.string
                { value = newLink.url
                , onInput = InputNewUrl
                , attrs = [ Forms.Placeholder "URL" ]
                }
            ]
        , td []
            [ Buttons.button
                { content = "sí"
                , onClick = Just CreateNewLink
                , attrs = []
                }
            ]
        ]
