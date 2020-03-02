module Page.Admin.CreateEvent exposing (Model, Msg(..), init, update, view)

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
import Models.Admin exposing (GigRequest, gigRequestDecoder)
import Models.Info exposing (Semester, Uniform, semesterDecoder)
import Request
import Route
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), mapLoaded, resultToRemote)



---- MODEL ----


type alias Model =
    { common : Common
    , createForm : RemoteData CreateEventForm
    , semesters : RemoteData (List Semester)
    , state : SubmissionState
    }


type alias CreateEventForm =
    { newEvent : NewEvent
    , newGig : NewGig
    , gigRequestId : Maybe Int
    }


type alias NewEvent =
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
    , repeat : RepeatPeriod
    , repeatUntil : String
    }


type alias NewGig =
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


type RepeatPeriod
    = NoRepeat
    | Daily
    | Weekly
    | BiWeekly
    | Monthly
    | Yearly


capitalizeFirstChar : String -> String
capitalizeFirstChar word =
    let
        firstChar =
            word |> String.slice 0 1 |> String.toUpper

        restOfWord =
            word |> String.slice 1 (String.length word)
    in
    firstChar ++ restOfWord


periodToString : RepeatPeriod -> String
periodToString period =
    case period of
        NoRepeat ->
            "No"

        Daily ->
            "Daily"

        Weekly ->
            "Weekly"

        BiWeekly ->
            "Biweekly"

        Monthly ->
            "Monthly"

        Yearly ->
            "Yearly"


stringToPeriod : String -> RepeatPeriod
stringToPeriod period =
    case period of
        "Daily" ->
            Daily

        "Weekly" ->
            Weekly

        "Biweekly" ->
            BiWeekly

        "Monthly" ->
            Monthly

        "Yearly" ->
            Yearly

        _ ->
            NoRepeat


allRepeatPeriods : List RepeatPeriod
allRepeatPeriods =
    [ NoRepeat, Daily, Weekly, BiWeekly, Monthly, Yearly ]


emptyEventForm : Common -> NewEvent
emptyEventForm common =
    { name = ""
    , semester = common.currentSemester.name
    , type_ = "Rehearsal"
    , callTime = ""
    , callDate = ""
    , releaseTime = ""
    , releaseDate = ""
    , points = Nothing
    , comments = ""
    , location = ""
    , gigCount = False
    , defaultAttend = True
    , repeat = NoRepeat
    , repeatUntil = ""
    }


emptyGigForm : NewGig
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


formFromGigRequest : Common -> GigRequest -> CreateEventForm
formFromGigRequest common gigRequest =
    { gigRequestId = Just gigRequest.id
    , newEvent =
        { name = gigRequest.name ++ " for " ++ gigRequest.organization
        , semester = common.currentSemester.name
        , type_ = "Volunteer Gig"
        , callTime = gigRequest.startTime |> twentyFourHourTimeFormatter common.timeZone
        , callDate = gigRequest.startTime |> hyphenDateFormatter common.timeZone
        , releaseTime = ""
        , releaseDate = ""
        , points = Just 5
        , comments = gigRequest.comments |> Maybe.withDefault ""
        , location = gigRequest.location
        , gigCount = True
        , defaultAttend = False
        , repeat = NoRepeat
        , repeatUntil = ""
        }
    , newGig =
        { performanceTime = gigRequest.startTime |> twentyFourHourTimeFormatter common.timeZone
        , uniform = Nothing
        , contactName = gigRequest.contactName
        , contactEmail = gigRequest.contactEmail
        , contactPhone = gigRequest.contactPhone
        , price = Nothing
        , public = False
        , summary = ""
        , description = ""
        }
    }


init : Common -> Maybe Int -> ( Model, Cmd Msg )
init common gigRequestId =
    ( { common = common
      , createForm =
            Loaded
                { newEvent = emptyEventForm common
                , newGig = emptyGigForm
                , gigRequestId = gigRequestId
                }
      , semesters = Loading
      , state = NotSentYet
      }
    , Cmd.batch <|
        [ loadSemesters common ]
            ++ (case gigRequestId of
                    Just requestId ->
                        [ loadGigRequest common requestId ]

                    Nothing ->
                        []
               )
    )



---- UPDATE ----


type Msg
    = OnLoadGigRequest (GreaseResult GigRequest)
    | OnLoadSemesters (GreaseResult (List Semester))
    | UpdateNewEventForm NewEvent
    | UpdateNewGigForm NewGig
    | CreateEvent
    | OnCreateEvent (GreaseResult Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadGigRequest result ->
            ( { model
                | createForm =
                    result
                        |> resultToRemote
                        |> mapLoaded (formFromGigRequest model.common)
              }
            , Cmd.none
            )

        OnLoadSemesters result ->
            ( { model | semesters = resultToRemote result }, Cmd.none )

        UpdateNewEventForm newEvent ->
            ( { model
                | createForm =
                    model.createForm
                        |> mapLoaded (\f -> { f | newEvent = newEvent })
              }
            , Cmd.none
            )

        UpdateNewGigForm newGig ->
            ( { model
                | createForm =
                    model.createForm
                        |> mapLoaded (\f -> { f | newGig = newGig })
              }
            , Cmd.none
            )

        CreateEvent ->
            case model.createForm of
                Loaded createForm ->
                    case createForm.gigRequestId of
                        Just requestId ->
                            ( { model | state = Sending }
                            , createEventFromGigRequest model.common createForm requestId
                            )

                        Nothing ->
                            ( { model | state = Sending }
                            , createEvent model.common createForm
                            )

                _ ->
                    ( model, Cmd.none )

        OnCreateEvent (Ok newId) ->
            ( model, Route.loadPage <| Route.Events { id = Just newId, tab = Nothing } )

        OnCreateEvent (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )



---- DATA ----


loadSemesters : Common -> Cmd Msg
loadSemesters common =
    Request.get common "/semesters" (Decode.list semesterDecoder)
        |> Task.attempt OnLoadSemesters


loadGigRequest : Common -> Int -> Cmd Msg
loadGigRequest common gigRequestId =
    let
        url =
            "/gig_requests/" ++ String.fromInt gigRequestId
    in
    Request.get common url gigRequestDecoder
        |> Task.attempt OnLoadGigRequest


createEventFromGigRequest : Common -> CreateEventForm -> Int -> Cmd Msg
createEventFromGigRequest common createForm requestId =
    let
        url =
            "/gig_requests/" ++ String.fromInt requestId ++ "/create_event"
    in
    Request.postReturningId common url (serializeEventForm common createForm)
        |> Task.attempt OnCreateEvent


createEvent : Common -> CreateEventForm -> Cmd Msg
createEvent common createForm =
    Request.postReturningId common "/events" (serializeEventForm common createForm)
        |> Task.attempt OnCreateEvent


serializeEventForm : Common -> CreateEventForm -> Encode.Value
serializeEventForm common createForm =
    let
        event =
            createForm.newEvent

        gig =
            createForm.newGig

        encodeDatetime dateString timeString =
            parseFormDateAndTimeString common dateString timeString
                |> Maybe.map (posixToMillis >> Encode.int)
                |> Maybe.withDefault Encode.null
    in
    Encode.object
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
        , ( "gig"
          , Encode.object
                [ ( "performanceTime", encodeDatetime event.callDate gig.performanceTime )
                , ( "uniform", gig.uniform |> Maybe.map (.id >> Encode.int) |> Maybe.withDefault Encode.null )
                , ( "contactName", Encode.string gig.contactName )
                , ( "contactEmail", Encode.string gig.contactEmail )
                , ( "contactPhone", Encode.string gig.contactPhone )
                , ( "price", Encode.null )
                , ( "public", Encode.bool gig.public )
                , ( "summary", Encode.string gig.summary )
                , ( "description", Encode.string gig.description )
                ]
          )
        , ( "repeat", event.repeat |> periodToString |> Encode.string )
        , ( "repeatUntil", encodeDatetime event.repeatUntil "00:00" )
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        []
        [ Basics.title "Create Event"
        , Basics.box
            [ model.createForm
                |> Basics.remoteContent (createEventForm model)
            ]
        ]


createEventForm : Model -> CreateEventForm -> Html Msg
createEventForm model createForm =
    form [ onSubmit CreateEvent ]
        [ Basics.columns
            [ leftColumnInEventForm createForm
            , middleColumnInEventForm model.common model.semesters createForm
            , rightColumnInEventForm createForm model.state
            ]
        ]


leftColumnInEventForm : CreateEventForm -> Html Msg
leftColumnInEventForm createForm =
    let
        newEvent =
            createForm.newEvent

        newGig =
            createForm.newGig
    in
    Basics.column
        [ textInput Forms.string
            { value = newEvent.name
            , onInput = \name -> UpdateNewEventForm { newEvent | name = name }
            , attrs =
                [ Forms.Title "Event Name"
                , Forms.Placeholder "Flashmobbing the HOMO SEX IS SIN people"
                , Forms.HelpText "Make it descriptive, make it short."
                , Forms.RequiredField True
                ]
            }
        , textInput Forms.string
            { value = newEvent.location
            , onInput = \location -> UpdateNewEventForm { newEvent | location = location }
            , attrs =
                [ Forms.Title "Event Location"
                , Forms.Placeholder "Your mom's house 😂"
                , Forms.HelpText "ha gottem"
                ]
            }
        , textInput Forms.date
            { value = newEvent.callDate
            , onInput = \callDate -> UpdateNewEventForm { newEvent | callDate = callDate }
            , attrs =
                [ Forms.Title "Date of Event"
                , Forms.RequiredField True
                ]
            }
        , textInput Forms.time
            { value = newEvent.callTime
            , onInput = \callTime -> UpdateNewEventForm { newEvent | callTime = callTime }
            , attrs =
                [ Forms.Title "Call Time"
                , Forms.HelpText "4:20 lamo"
                , Forms.RequiredField True
                ]
            }
        , textInput Forms.time
            { value = newGig.performanceTime
            , onInput = \performanceTime -> UpdateNewGigForm { newGig | performanceTime = performanceTime }
            , attrs =
                [ Forms.Title "Event Time"
                , Forms.HelpText "4:21 lamo"
                ]
            }
        , textInput Forms.time
            { value = newEvent.releaseTime
            , onInput = \releaseTime -> UpdateNewEventForm { newEvent | releaseTime = releaseTime }
            , attrs =
                [ Forms.Title "Release Time"
                , Forms.HelpText "4:22 lamo"
                ]
            }
        , textInput Forms.date
            { value = newEvent.releaseDate
            , onInput = \releaseDate -> UpdateNewEventForm { newEvent | releaseDate = releaseDate }
            , attrs = [ Forms.Title "Release Date" ]
            }
        , textInput Forms.int
            { value = newEvent.points
            , onInput = \points -> UpdateNewEventForm { newEvent | points = points }
            , attrs =
                [ Forms.Title "How many points is this worth?"
                , Forms.Placeholder "69"
                ]
            }
        ]


middleColumnInEventForm : Common -> RemoteData (List Semester) -> CreateEventForm -> Html Msg
middleColumnInEventForm common remoteSemesters createForm =
    let
        newEvent =
            createForm.newEvent

        newGig =
            createForm.newGig

        uniformInputType =
            { toString = Maybe.map .name >> Maybe.withDefault "(no uniform)"
            , fromString = \name -> common.info.uniforms |> List.find (\u -> u.name == name)
            , textType = Forms.Text
            }
    in
    Basics.column
        [ radioInput identity
            { values = common.info.eventTypes |> List.map .name
            , selected = newEvent.type_
            , onInput = \type_ -> UpdateNewEventForm { newEvent | type_ = type_ }
            , attrs = [ Forms.Title "Event Type" ]
            }
        , selectInput Forms.string
            { values =
                remoteSemesters
                    |> Utils.remoteToMaybe
                    |> Maybe.map (List.map .name)
                    |> Maybe.withDefault [ common.currentSemester.name ]
            , selected = newEvent.semester
            , onInput = \semester -> UpdateNewEventForm { newEvent | semester = semester }
            , attrs =
                [ Forms.Title "Semester"
                , Forms.IsLoading (remoteSemesters == Loading)
                ]
            }
        , selectInput uniformInputType
            { values = Nothing :: (common.info.uniforms |> List.map Just)
            , selected = newGig.uniform
            , onInput = \uniform -> UpdateNewGigForm { newGig | uniform = uniform }
            , attrs = [ Forms.Title "Uniform" ]
            }
        , textareaInput
            { value = newEvent.comments
            , onInput = \comments -> UpdateNewEventForm { newEvent | comments = comments }
            , attrs =
                [ Forms.Title "Event Summary"
                , Forms.Placeholder "We're gonna get in there, we're gonna use our mouths, and we're gonna get out."
                ]
            }
        ]


rightColumnInEventForm : CreateEventForm -> SubmissionState -> Html Msg
rightColumnInEventForm createForm state =
    let
        newEvent =
            createForm.newEvent

        newGig =
            createForm.newGig

        isPublicInput =
            checkboxInput
                { content = "This event is public, so I want it to show up on the external site"
                , isChecked = newGig.public
                , onChange = \public -> UpdateNewGigForm { newGig | public = public }
                }

        publicEventInputs =
            if not newGig.public then
                [ isPublicInput ]

            else
                [ isPublicInput
                , textInput Forms.string
                    { value = newGig.summary
                    , onInput = \summary -> UpdateNewGigForm { newGig | summary = summary }
                    , attrs =
                        [ Forms.Title "Public Summary"
                        , Forms.HelpText "Careful, real people will see this"
                        , Forms.Placeholder "Friends? Countrymen? Bueller?"
                        ]
                    }
                , textareaInput
                    { value = newGig.description
                    , onInput = \description -> UpdateNewGigForm { newGig | description = description }
                    , attrs =
                        [ Forms.Title "Public Description"
                        , Forms.HelpText "Careful, real people will see this"
                        , Forms.Placeholder "We the people, in order to kick a more perfect ass, I don't know where this is going"
                        ]
                    }
                ]

        repeatInputType =
            { toString = periodToString >> capitalizeFirstChar
            , fromString = stringToPeriod
            , textType = Forms.Text
            }
    in
    Basics.column
        (publicEventInputs
            ++ [ checkboxInput
                    { content = "No one has to come to this event (forum, fundatory, etc)"
                    , isChecked = not newEvent.defaultAttend
                    , onChange = \defaultNotAttend -> UpdateNewEventForm { newEvent | defaultAttend = not defaultNotAttend }
                    }
               , checkboxInput
                    { content = "This event counts as a volunteer gig"
                    , isChecked = newEvent.gigCount
                    , onChange = \gigCount -> UpdateNewEventForm { newEvent | gigCount = gigCount }
                    }
               , selectInput repeatInputType
                    { values = allRepeatPeriods
                    , selected = newEvent.repeat
                    , onInput = \period -> UpdateNewEventForm { newEvent | repeat = period }
                    , attrs = [ Forms.Title "Repeat" ]
                    }
               , textInput Forms.date
                    { value = newEvent.repeatUntil
                    , onInput = \repeatUntil -> UpdateNewEventForm { newEvent | repeatUntil = repeatUntil }
                    , attrs =
                        [ Forms.Title "Repeat Until"
                        , Forms.RequiredField (newEvent.repeat /= NoRepeat)
                        ]
                    }
               , br [] []
               , Buttons.submit
                    { content = "Yeehaw"
                    , attrs = [ Buttons.Color Buttons.IsPrimary, Buttons.IsLoading (state == Sending) ]
                    }
               , case state of
                    ErrorSending error ->
                        Basics.errorBox error

                    _ ->
                        text ""
               ]
        )
