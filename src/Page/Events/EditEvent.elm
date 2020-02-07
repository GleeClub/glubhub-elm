module Page.Events.EditEvent exposing (InternalMsg, Model, Msg, Translator, init, translator, update, view)

import Components.Basics as Basics
import Components.Buttons as Buttons
import Components.Forms as Forms exposing (checkboxInput, radioInput, selectInput, textInput, textareaInput)
import Datetime exposing (hyphenDateFormatter, parseFormDateAndTimeString, twentyFourHourTimeFormatter)
import Error exposing (GreaseResult)
import Html exposing (Html, br, div, form, text)
import Html.Events exposing (onSubmit)
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
        [ Basics.centeredTitle "Edit Event"
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
        [ textInput Forms.string
            { value = event.name
            , onInput = \name -> ForSelf <| UpdateEventForm { event | name = name }
            , attrs =
                [ Forms.Title "Event Name"
                , Forms.Placeholder "Flashmobbing the HOMO SEX IS SIN people"
                , Forms.HelpText "Make it descriptive, make it short."
                , Forms.RequiredField True
                ]
            }
        , textInput Forms.string
            { value = event.location
            , onInput = \location -> ForSelf <| UpdateEventForm { event | location = location }
            , attrs =
                [ Forms.Title "Event Location"
                , Forms.Placeholder "Your mom's house 😂"
                , Forms.HelpText "ha gottem"
                ]
            }
        , textInput Forms.date
            { value = event.callDate
            , onInput = \callDate -> ForSelf <| UpdateEventForm { event | callDate = callDate }
            , attrs =
                [ Forms.Title "Date of Event"
                , Forms.RequiredField True
                ]
            }
        , textInput Forms.time
            { value = event.callTime
            , onInput = \callTime -> ForSelf <| UpdateEventForm { event | callTime = callTime }
            , attrs =
                [ Forms.Title "Call Time"
                , Forms.HelpText "4:20 lamo"
                , Forms.RequiredField True
                ]
            }
        , textInput Forms.time
            { value = gig.performanceTime
            , onInput = \performanceTime -> ForSelf <| UpdateGigForm { gig | performanceTime = performanceTime }
            , attrs =
                [ Forms.Title "Event Time"
                , Forms.HelpText "4:21 lamo"
                ]
            }
        , textInput Forms.time
            { value = event.releaseTime
            , onInput = \releaseTime -> ForSelf <| UpdateEventForm { event | releaseTime = releaseTime }
            , attrs =
                [ Forms.Title "Release Time"
                , Forms.HelpText "4:22 lamo"
                ]
            }
        , textInput Forms.date
            { value = event.releaseDate
            , onInput = \releaseDate -> ForSelf <| UpdateEventForm { event | releaseDate = releaseDate }
            , attrs = [ Forms.Title "Release Date" ]
            }
        , textInput Forms.int
            { value = event.points
            , onInput = \points -> ForSelf <| UpdateEventForm { event | points = points }
            , attrs =
                [ Forms.Title "How many points is this worth?"
                , Forms.Placeholder "69"
                ]
            }
        ]


middleColumnInEventForm : Model -> Html Msg
middleColumnInEventForm model =
    let
        event =
            model.event

        gig =
            model.gig

        uniformInputType =
            { toString = Maybe.map .name >> Maybe.withDefault "(no uniform)"
            , fromString = \name -> model.common.info.uniforms |> List.find (\u -> u.name == name)
            , textType = Forms.Text
            }
    in
    Basics.column
        [ radioInput identity
            { values = model.common.info.eventTypes |> List.map .name
            , selected = event.type_
            , onInput = \type_ -> ForSelf <| UpdateEventForm { event | type_ = type_ }
            , attrs = [ Forms.Title "Event Type" ]
            }
        , selectInput Forms.string
            { values =
                model.semesters
                    |> Utils.remoteToMaybe
                    |> Maybe.map (List.map .name)
                    |> Maybe.withDefault [ model.common.currentSemester.name ]
            , selected = event.semester
            , onInput = \semester -> ForSelf <| UpdateEventForm { event | semester = semester }
            , attrs =
                [ Forms.Title "Semester"
                , Forms.IsLoading (model.semesters == Loading)
                ]
            }
        , selectInput uniformInputType
            { values = Nothing :: (model.common.info.uniforms |> List.map Just)
            , selected = gig.uniform
            , onInput = \uniform -> ForSelf <| UpdateGigForm { gig | uniform = uniform }
            , attrs = [ Forms.Title "Uniform" ]
            }
        , textareaInput
            { value = event.comments
            , onInput = \comments -> ForSelf <| UpdateEventForm { event | comments = comments }
            , attrs =
                [ Forms.Title "Event Summary"
                , Forms.Placeholder "We're gonna get in there, we're gonna use our mouths, and we're gonna get out."
                ]
            }
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
                , textInput Forms.string
                    { value = gig.summary
                    , onInput = \summary -> ForSelf <| UpdateGigForm { gig | summary = summary }
                    , attrs =
                        [ Forms.Title "Public Summary"
                        , Forms.HelpText "Careful, real people will see this"
                        , Forms.Placeholder "Friends? Countrymen? Bueller?"
                        ]
                    }
                , textareaInput
                    { value = gig.description
                    , onInput = \description -> ForSelf <| UpdateGigForm { gig | description = description }
                    , attrs =
                        [ Forms.Title "Public Description"
                        , Forms.HelpText "Careful, real people will see this"
                        , Forms.Placeholder "We the people, in order to kick a more perfect ass, I don't know where this is going"
                        ]
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
               , br [] []
               , Buttons.submit
                    { content = "Update"
                    , attrs =
                        [ Buttons.Color Buttons.IsPrimary
                        , Buttons.IsLoading (state == Sending)
                        ]
                    }
               , case state of
                    ErrorSending error ->
                        Basics.errorBox error

                    _ ->
                        text ""
               ]
        )
