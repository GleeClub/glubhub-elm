module Page.Admin.CreateEvent exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, form, h1, i, img, input, label, p, section, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import List.Extra exposing (groupWhile)
import Models.Document exposing (DocumentLink, documentLinkDecoder)
import Models.Event exposing (EventAttendee, Member, eventAttendeeDecoder)
import Route exposing (Route)
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), alert, deleteRequest, getRequest, postRequest)
import Iso8601
import Time exposing (Posix)

---- MODEL ----


type alias Model =
    { common : Common
    , newEvent : NewEventForm
    , state : SubmissionState
    }


type alias NewEventForm =
    { name : String
    , semester : String
    , type_ : String
    , callTime : Posix
    , releaseTime : Maybe Posix
    , points : Int
    , comments : Maybe String
    , location : Maybe String
    , gigCount : Maybe Bool
    , defaultAttend : Bool
    , repeat : String
    , repeatUntil : Maybe Posix
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , links = Loading
      , state = NotSentYet
      , newLink = { name = "", url = "" }
      }
    , loadDocumentLinks common
    )


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
        OnLoadLinks (Ok links) ->
            ( { model | links = Loaded links }, Cmd.none )

        OnLoadLinks (Err error) ->
            ( { model | links = Failure error }, Cmd.none )

        OnChangeLink result ->
            ( { model
                | state =
                    case result of
                        Ok _ ->
                            NotSentYet

                        Err error ->
                            ErrorSending error
              }
            , Cmd.none
            )

        UpdateLink link linkIndex ->
            case model.links of
                Loaded links ->
                    let
                        linkMapper =
                            List.indexedMap
                                (\index oldLink ->
                                    if index == linkIndex then
                                        link

                                    else
                                        oldLink
                                )
                    in
                    ( { model | links = Loaded (links |> linkMapper) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

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
                        | links = Loaded (links |> List.Extra.removeAt index)
                        , state = Sending
                      }
                    , updateDocumentLink model.common link
                    )

                _ ->
                    ( model, Cmd.none )

        InputNewName name ->
            ( { model | newLink = { name = name, url = model.newLink.url } }, Cmd.none )

        InputNewUrl url ->
            ( { model | newLink = { name = model.newLink.name, url = url } }, Cmd.none )

        CreateNewLink ->
            ( { model | newLink = { name = "", url = "" }, state = Sending }
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


serializeEventForm : NewEventForm -> Encode.Value
serializeEventForm newEvent =
    Encode.object
        [ ( "name", Encode.string newEvent.name )
        , ( "semester", Encode.string newEvent.semester )
        , ( "type", Encode.string newEvent.type_ )
        , ( "callTime", Encode.string )
        ]


pub struct NewEvent {
    pub name: String,
    pub semester: String,
    #[rename = "type"]
    #[serde(rename = "type")]
    pub type_: String,
    #[serde(rename = "callTime")]
    pub call_time: NaiveDateTime,
    #[serde(rename = "releaseTime")]
    pub release_time: Option<NaiveDateTime>,
    pub points: i32,
    #[serde(deserialize_with = "deser_opt_string")]
    pub comments: Option<String>,
    #[serde(deserialize_with = "deser_opt_string")]
    pub location: Option<String>,
    #[serde(default, rename = "gigCount")]
    pub gig_count: Option<bool>,
    #[serde(rename = "defaultAttend")]
    pub default_attend: bool,
    pub repeat: String,
    #[serde(rename = "repeatUntil")]
    pub repeat_until: Option<NaiveDate>,
}


---- VIEW ----


view : Model -> Html Msg
view model =
    Basics.column
        [ Basics.title "Document Links"
        , Basics.box
            [ model.links |> Basics.remoteContent (\links -> div [] []) ]
        ]


linkRow : DocumentLink -> Html Msg
linkRow link =
    div []
        [ text <| link.name ++ " "
        , span [] []
        ]


type alias TextInput =
    { title : String
    , helpText : Maybe String
    , value : String
    , placeholder : String
    , onInput : String -> Msg
    }


textInput : TextInput -> Html Msg
textInput data =
    fieldWrapper data
        [ input
            [ class "input"
            , type_ "text"
            , placeholder data.placeholder
            , value data.value
            , onInput data.onInput
            ]
            []
        ]


type alias FieldInput a =
    { a
        | title : String
        , helpText : Maybe String
    }


fieldWrapper : FieldInput a -> List (Html Msg) -> Html Msg
fieldWrapper field content =
    div [ class "field" ]
        [ label [ class "label" ] [ text field.title ]
        , div [ class "control" ] content
        , field.helpText
            |> Maybe.map (\help -> p [ class "help" ] [ text help ])
            |> Maybe.withDefault (text "")
        ]



-- <template>
--     <div id="create-event" class="columns">
--         <div class="column">
--             <div class="field">
--                 <label class="label">Event Name</label>
--                 <div class="control">
--                     <input class="input" type="text" placeholder="Flashmobbing the HOMO SEX IS SIN people">
--                 </div>
--                 <p class="help">Make it descriptive, make it short.</p>
--             </div>
--             <div class="field">
--                 <label class="label">Event Location</label>
--                 <div class="control">
--                     <input class="input" type="text" placeholder="Your mom's house 😂">
--                 </div>
--                 <p class="help">ha gottem</p>
--             </div>
--             <div class="field">
--                 <label class="label">Date of Event</label>
--                 <div class="control">
--                     <input class="input" type="date">
--                 </div>
--                 <p class="help"></p>
--             </div>
--             <div class="field">
--                 <label class="label">Event Time</label>
--                 <div class="control">
--                     <input class="input" type="time">
--                 </div>
--                 <p class="help">4:20 lamo</p>
--             </div>
--             <div class="field">
--                 <label class="label">Call Time</label>
--                 <div class="control">
--                     <input class="input" type="time">
--                 </div>
--                 <p class="help">4:20 lamo</p>
--             </div>
--             <div class="field">
--                 <label class="label">Release Time</label>
--                 <div class="control">
--                     <input class="input" type="time">
--                 </div>
--                 <p class="help">4:20 lamo</p>
--             </div>
--             <div class="field">
--             <label class="label">Release Date</label>
--             <div class="control">
--                 <input class="input" type="date">
--             </div>
--             <p class="help"></p>
--             </div>
--             <div class="field">
--             <label class="label">How many points is this worth?</label>
--             <div class="control">
--                 <input class="input" type="text" placeholder="69">
--             </div>
--             <p class="help"></p>
--             </div>
--         </div>
--         <div class="column">
--             <div class="field">
--                 <label class="label">Event Type</label>
--                 <div class="control">
--                     <label class="radio">
--                         <input type="radio" name="gigType" checked>Volunteer Gig
--                     </label><br>
--                     <label class="radio">
--                         <input type="radio" name="gigType">Tutti Gig
--                     </label><br>
--                     <label class="radio">
--                         <input type="radio" name="gigType">Sectional
--                     </label><br>
--                     <label class="radio">
--                         <input type="radio" name="gigType">Rehearsal
--                     </label><br>
--                     <label class="radio">
--                         <input type="radio" name="gigType">Ombuds
--                     </label><br>
--                     <label class="radio">
--                         <input type="radio" name="gigType">Other
--                     </label>
--                 </div>
--             </div>
--             <div class="field">
--                 <label class="label">Semester</label>
--                 <div class="select is-loading control">
--                     <select>
--                         <option>Fall 2019</option>
--                         <option>Spring 2020</option>
--                     </select>
--                 </div>
--             </div>
--             <div class="field">
--             <label class="label">Event Summary</label>
--             <div class="control">
--                 <textarea class="textarea" placeholder="We're gonna get in there, we're gonna use our mouths, and we're gonna get out."></textarea>
--             </div>
--             <p class="help"></p>
--             </div>
--         </div>
--         <div class="column">
--             <div class="field">
--                 <div class="control">
--                     <label class="checkbox">
--                         <input type="checkbox" checked>This event is public, so I want it to show up on the external site
--                     </label>
--                 </div>
--             </div>
--             <div class="field">
--                 <div class="control">
--                     <label class="checkbox">
--                         <input type="checkbox" checked>This event is for all sections
--                     </label>
--                     <label class="checkbox">
--                         <input type="checkbox" checked>This event is for the T1 section
--                     </label>
--                     <label class="checkbox">
--                         <input type="checkbox" checked>This event is for the T2 section
--                     </label>
--                     <label class="checkbox">
--                         <input type="checkbox" checked>This event is for the B1 section
--                     </label>
--                     <label class="checkbox">
--                         <input type="checkbox" checked>This event is for the B2 section
--                     </label>
--                 </div>
--             </div>
--             <div class="field">
--                 <div class="control">
--                     <label class="checkbox">
--                         <input type="checkbox">No one has to come to this event (forum, fundatory, etc)
--                     </label>
--                 </div>
--             </div>
--             <div class="field">
--                 <div class="control">
--                     <label class="checkbox">
--                         <input type="checkbox">This event counts as a volunteer gig
--                     </label>
--                 </div>
--             </div>
--             <div class="field">
--                 <div class="control">
--                     <label class="checkbox">
--                         <input type="checkbox">Members are required to attend (overrides any other setting)
--                     </label>
--                 </div>
--             </div>
--             <div class="field">
--                 <label class="label">Repeat</label>
--                 <div class="select is-loading control">
--                     <select>
--                         <option>None</option>
--                         <option>Weekly</option>
--                         <option>Monthly</option>
--                     </select>
--                 </div>
--             </div>
--             <div class="field">
--             <label class="label">Repeat until</label>
--             <div class="control">
--                 <input class="input" type="date">
--             </div>
--             <p class="help"></p>
--             </div>
--         </div>
--     </div>
-- </template>
