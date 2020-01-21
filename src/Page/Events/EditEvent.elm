module Page.Events.EditEvent exposing (InternalMsg, Model, Msg, Translator, init, translator, update, view)

import Components.Basics as Basics
import Components.Forms exposing (checkboxInput, dateInput, fieldWrapper, selectInput, textInput, textareaInput, timeInput)
import Datetime exposing (hyphenDateFormatter, parseFormDateAndTimeString, twentyFourHourTimeFormatter)
import Error exposing (GreaseResult)
import Html exposing (Html, br, button, div, form, input, label, text, textarea)
import Html.Attributes exposing (checked, class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (string)
import Json.Encode as Encode
import List.Extra as List
import Models.Event exposing (Event, eventDecoder)
import Models.Info exposing (Semester, Uniform, semesterDecoder)
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), getRequest, postRequest, resultToRemote)



---- MODEL ----


type alias Model =
    { common : Common
    , eventId : Int
    , event : EditEventForm
    , gig : EditGigForm
    , semesters : RemoteData (List Semester)
    , state : SubmissionState
    }


type alias EditEventForm =
    { name : String
    , semester : String
    , type_ : String
    , callTime : String
    , callDate : String
    , releaseTime : String
    , releaseDate : String
    , points : Maybe Int
    , comments : String
    , location : String
    , gigCount : Bool
    , defaultAttend : Bool
    }


type alias EditGigForm =
    { performanceTime : String
    , uniform : Maybe Uniform
    , contactName : String
    , contactEmail : String
    , contactPhone : String
    , price : Maybe Int
    , public : Bool
    , summary : String
    , description : String
    }


eventFormFromEvent : Common -> Event -> EditEventForm
eventFormFromEvent common event =
    { name = event.name
    , semester = event.semester
    , type_ = event.type_
    , callTime = event.callTime |> twentyFourHourTimeFormatter common.timeZone
    , callDate = event.callTime |> hyphenDateFormatter common.timeZone
    , releaseTime = event.callTime |> twentyFourHourTimeFormatter common.timeZone
    , releaseDate = event.callTime |> hyphenDateFormatter common.timeZone
    , points = Just event.points
    , comments = event.comments |> Maybe.withDefault ""
    , location = event.location |> Maybe.withDefault ""
    , gigCount = event.gigCount
    , defaultAttend = event.defaultAttend
    }


gigFormFromEvent : Common -> Event -> EditGigForm
gigFormFromEvent common event =
    case event.gig of
        Just gig ->
            { performanceTime = gig.performanceTime |> twentyFourHourTimeFormatter common.timeZone
            , uniform = common.info.uniforms |> List.find (\u -> u.id == gig.uniform)
            , contactName = gig.contactName |> Maybe.withDefault ""
            , contactEmail = gig.contactEmail |> Maybe.withDefault ""
            , contactPhone = gig.contactPhone |> Maybe.withDefault ""
            , price = gig.price
            , public = gig.public
            , summary = gig.summary |> Maybe.withDefault ""
            , description = gig.description |> Maybe.withDefault ""
            }

        Nothing ->
            emptyGigForm


emptyGigForm : EditGigForm
emptyGigForm =
    { performanceTime = ""
    , uniform = Nothing
    , contactName = ""
    , contactEmail = ""
    , contactPhone = ""
    , price = Nothing
    , public = False
    , summary = ""
    , description = ""
    }


init : Common -> Event -> ( Model, Cmd Msg )
init common event =
    ( { common = common
      , eventId = event.id
      , event = event |> eventFormFromEvent common
      , gig = event |> gigFormFromEvent common
      , semesters = Loading
      , state = NotSentYet
      }
    , loadSemesters common
    )



---- UPDATE ----


type Msg
    = ForSelf InternalMsg
    | ForParent OutMsg


type InternalMsg
    = OnLoadSemesters (GreaseResult (List Semester))
    | UpdateEventForm EditEventForm
    | UpdateGigForm EditGigForm
    | UpdateEvent
    | OnUpdateEvent (GreaseResult Event)


type OutMsg
    = PropagateUpdate Event


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onPropagateUpdate : Event -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator dictionary msg =
    case msg of
        ForSelf internal ->
            dictionary.onInternalMessage internal

        ForParent (PropagateUpdate event) ->
            dictionary.onPropagateUpdate event


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadSemesters result ->
            ( { model | semesters = resultToRemote result }, Cmd.none )

        UpdateEventForm eventForm ->
            ( { model | event = eventForm }, Cmd.none )

        UpdateGigForm gigForm ->
            ( { model | gig = gigForm }, Cmd.none )

        UpdateEvent ->
            ( { model | state = Sending }, updateEvent model )

        OnUpdateEvent (Ok updatedEvent) ->
            ( model, Task.perform (\_ -> ForParent <| PropagateUpdate updatedEvent) (Task.succeed ()) )

        OnUpdateEvent (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )



---- DATA ----


loadSemesters : Common -> Cmd Msg
loadSemesters common =
    getRequest common "/semesters" (Decode.list semesterDecoder)
        |> Task.attempt (ForSelf << OnLoadSemesters)


updateEvent : Model -> Cmd Msg
updateEvent model =
    let
        url =
            "/events/" ++ String.fromInt model.eventId

        body =
            serializeEventForm model.common model.event model.gig
    in
    postRequest model.common url body
        |> Task.andThen (\_ -> getRequest model.common url eventDecoder)
        |> Task.attempt (ForSelf << OnUpdateEvent)


serializeEventForm : Common -> EditEventForm -> EditGigForm -> Encode.Value
serializeEventForm common event gig =
    let
        encodeDatetime dateString timeString =
            parseFormDateAndTimeString common dateString timeString
                |> Maybe.map (posixToMillis >> Encode.int)
                |> Maybe.withDefault Encode.null
    in
    Encode.object
        -- event fields
        [ ( "name", Encode.string event.name )
        , ( "semester", Encode.string event.semester )
        , ( "type", Encode.string event.type_ )
        , ( "callTime", encodeDatetime event.callDate event.callTime )
        , ( "releaseTime", encodeDatetime event.releaseDate event.releaseTime )
        , ( "points", event.points |> Maybe.withDefault 5 |> Encode.int )
        , ( "comments", Encode.string event.comments )
        , ( "location", Encode.string event.location )
        , ( "gigCount", Encode.bool event.gigCount )
        , ( "defaultAttend", Encode.bool event.defaultAttend )

        -- gig fields
        , ( "performanceTime", encodeDatetime event.callDate gig.performanceTime )
        , ( "uniform", gig.uniform |> Maybe.map (.id >> Encode.int) |> Maybe.withDefault Encode.null )
        , ( "contactName", Encode.string gig.contactName )
        , ( "contactEmail", Encode.string gig.contactEmail )
        , ( "contactPhone", Encode.string gig.contactPhone )
        , ( "price", gig.price |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
        , ( "public", Encode.bool gig.public )
        , ( "summary", Encode.string gig.summary )
        , ( "description", Encode.string gig.description )
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Edit Event"
        , form [ onSubmit <| ForSelf UpdateEvent ]
            [ leftColumnInEventForm model.event model.gig
            , middleColumnInEventForm model
            , rightColumnInEventForm model.state model.event model.gig
            , Basics.submissionStateBox model.state
            ]
        ]


leftColumnInEventForm : EditEventForm -> EditGigForm -> Html Msg
leftColumnInEventForm event gig =
    Basics.column
        [ textInput
            { title = "Event Name"
            , value = event.name
            , placeholder = "Flashmobbing the HOMO SEX IS SIN people"
            , helpText = Just "Make it descriptive, make it short."
            , required = True
            , onInput = \name -> ForSelf <| UpdateEventForm { event | name = name }
            }
        , textInput
            { title = "Event Location"
            , value = event.location
            , placeholder = "Your mom's house 😂"
            , helpText = Just "ha gottem"
            , required = False
            , onInput = \location -> ForSelf <| UpdateEventForm { event | location = location }
            }
        , dateInput
            { title = "Date of Event"
            , value = event.callDate
            , placeholder = ""
            , helpText = Nothing
            , required = True
            , onInput = \callDate -> ForSelf <| UpdateEventForm { event | callDate = callDate }
            }
        , timeInput
            { title = "Call Time"
            , value = event.callTime
            , placeholder = ""
            , helpText = Just "4:20 lamo"
            , required = True
            , onInput = \callTime -> ForSelf <| UpdateEventForm { event | callTime = callTime }
            }
        , timeInput
            { title = "Event Time"
            , value = gig.performanceTime
            , placeholder = ""
            , helpText = Just "4:21 lamo"
            , required = False
            , onInput = \performanceTime -> ForSelf <| UpdateGigForm { gig | performanceTime = performanceTime }
            }
        , timeInput
            { title = "Release Time"
            , value = event.releaseTime
            , placeholder = ""
            , helpText = Just "4:22 lamo"
            , required = False
            , onInput = \releaseTime -> ForSelf <| UpdateEventForm { event | releaseTime = releaseTime }
            }
        , dateInput
            { title = "Release Date"
            , value = event.releaseDate
            , placeholder = ""
            , helpText = Nothing
            , required = False
            , onInput = \releaseDate -> ForSelf <| UpdateEventForm { event | releaseDate = releaseDate }
            }
        , textInput
            { title = "How many points is this worth?"
            , value = event.points |> Maybe.map String.fromInt |> Maybe.withDefault ""
            , placeholder = "69"
            , helpText = Nothing
            , required = False
            , onInput = \points -> ForSelf <| UpdateEventForm { event | points = points |> String.toInt }
            }
        ]


middleColumnInEventForm : Model -> Html Msg
middleColumnInEventForm model =
    let
        event =
            model.event

        gig =
            model.gig

        eventTypeOption selectedType eventType =
            label [ class "radio" ]
                [ input
                    [ type_ "radio"
                    , checked <| eventType.name == selectedType
                    , onClick <| ForSelf <| UpdateEventForm { event | type_ = eventType.name }
                    ]
                    []
                , text <| " " ++ eventType.name
                ]
    in
    Basics.column
        [ fieldWrapper { title = "Event Type", helpText = Nothing } <|
            (model.common.info.eventTypes
                |> List.map (eventTypeOption event.type_)
                |> List.intersperse (br [] [])
            )
        , selectInput
            { title = "Semester"
            , helpText = Nothing
            , values =
                model.semesters
                    |> Utils.remoteToMaybe
                    |> Maybe.map (List.map .name)
                    |> Maybe.withDefault [ model.common.currentSemester.name ]
            , render = \name -> ( name, name )
            , loading = model.semesters == Loading
            , selected = (==) event.semester
            , onSelect = \semester -> ForSelf <| UpdateEventForm { event | semester = semester }
            }
        , selectInput
            { title = "Uniform"
            , helpText = Nothing
            , values = Nothing :: (model.common.info.uniforms |> List.map Just)
            , render =
                Maybe.map (\u -> ( u.name, u.name ))
                    >> Maybe.withDefault ( "", "(no uniform)" )
            , loading = False
            , selected =
                \uniform ->
                    case ( uniform, model.gig.uniform ) of
                        ( Just u, Just selected ) ->
                            u.id == selected.id

                        ( Nothing, Nothing ) ->
                            True

                        _ ->
                            False
            , onSelect =
                \name ->
                    ForSelf <|
                        UpdateGigForm
                            { gig
                                | uniform = model.common.info.uniforms |> List.find (\u -> u.name == name)
                            }
            }
        , fieldWrapper { title = "Event Summary", helpText = Nothing } <|
            [ textarea
                [ class "textarea"
                , placeholder "We're gonna get in there, we're gonna use our mouths, and we're gonna get out."
                , value event.comments
                , onInput (\comments -> ForSelf <| UpdateEventForm { event | comments = comments })
                ]
                []
            ]
        ]


rightColumnInEventForm : SubmissionState -> EditEventForm -> EditGigForm -> Html Msg
rightColumnInEventForm state event gig =
    let
        isPublicInput =
            checkboxInput
                { content = "This event is public, so I want it to show up on the external site"
                , isChecked = gig.public
                , onChange = \public -> ForSelf <| UpdateGigForm { gig | public = public }
                }

        publicEventInputs =
            if not gig.public then
                [ isPublicInput ]

            else
                [ isPublicInput
                , textInput
                    { title = "Public Summary"
                    , value = gig.summary
                    , placeholder = "Friends? Countrymen? Bueller?"
                    , helpText = Just "Careful, real people will see this"
                    , required = False
                    , onInput = \summary -> ForSelf <| UpdateGigForm { gig | summary = summary }
                    }
                , textareaInput
                    { title = "Public Description"
                    , value = gig.description
                    , placeholder = "We the people, in order to kick a more perfect ass, I don;t know where this is going"
                    , helpText = Just "Careful, real people will see this"
                    , required = False
                    , onInput = \description -> ForSelf <| UpdateGigForm { gig | description = description }
                    }
                ]
    in
    Basics.column
        (publicEventInputs
            ++ [ checkboxInput
                    { content = "No one has to come to this event (forum, fundatory, etc)"
                    , isChecked = not event.defaultAttend
                    , onChange = \defaultNotAttend -> ForSelf <| UpdateEventForm { event | defaultAttend = not defaultNotAttend }
                    }
               , checkboxInput
                    { content = "This event counts as a volunteer gig"
                    , isChecked = event.gigCount
                    , onChange = \gigCount -> ForSelf <| UpdateEventForm { event | gigCount = gigCount }
                    }
               , br [] []
               , button
                    [ type_ "submit"
                    , class <| "button is-primary" ++ Utils.isLoadingClass (state == Sending)
                    ]
                    [ text "Update" ]
               , case state of
                    ErrorSending error ->
                        Basics.errorBox error

                    _ ->
                        text ""
               ]
        )
