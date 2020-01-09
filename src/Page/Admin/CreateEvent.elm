module Page.Admin.CreateEvent exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Components.Forms exposing (checkboxInput, dateInput, fieldWrapper, textInput, textareaInput, timeInput)
import Datetime exposing (hyphenDateFormatter, parseFormDateAndTimeString, twentyFourHourTimeFormatter)
import Error exposing (GreaseResult)
import Html exposing (Html, br, button, div, form, input, label, option, select, text, textarea)
import Html.Attributes exposing (checked, class, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra exposing (isNothing)
import Models.Admin exposing (GigRequest, gigRequestDecoder)
import Models.Info exposing (Semester, Uniform, semesterDecoder)
import Route
import Task
import Time exposing (posixToMillis)
import Utils exposing (Common, RemoteData(..), SubmissionState(..), getRequest, mapLoaded, postRequestFull, resultToRemote)



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
            "no"

        Daily ->
            "daily"

        Weekly ->
            "weekly"

        BiWeekly ->
            "biweekly"

        Monthly ->
            "monthly"

        Yearly ->
            "yearly"


stringToPeriod : String -> RepeatPeriod
stringToPeriod period =
    case period of
        "daily" ->
            Daily

        "weekly" ->
            Weekly

        "biweekly" ->
            BiWeekly

        "monthly" ->
            Monthly

        "yearly" ->
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
    getRequest common "/semesters" (Decode.list semesterDecoder)
        |> Task.attempt OnLoadSemesters


loadGigRequest : Common -> Int -> Cmd Msg
loadGigRequest common gigRequestId =
    let
        url =
            "/gig_requests/" ++ String.fromInt gigRequestId
    in
    getRequest common url gigRequestDecoder
        |> Task.attempt OnLoadGigRequest


createEventFromGigRequest : Common -> CreateEventForm -> Int -> Cmd Msg
createEventFromGigRequest common createForm requestId =
    let
        url =
            "/gig_requests/" ++ String.fromInt requestId ++ "/create_event"
    in
    postRequestFull common url (serializeEventForm common createForm) (Decode.field "id" Decode.int)
        |> Task.attempt OnCreateEvent


createEvent : Common -> CreateEventForm -> Cmd Msg
createEvent common createForm =
    postRequestFull common "/events" (serializeEventForm common createForm) (Decode.field "id" Decode.int)
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
        , ( "price", Encode.null )
        , ( "public", Encode.bool gig.public )
        , ( "summary", Encode.string gig.summary )
        , ( "description", Encode.string gig.description )
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
                |> Basics.remoteContent (createEventForm model.common model.semesters)
            ]
        ]


createEventForm : Common -> RemoteData (List Semester) -> CreateEventForm -> Html Msg
createEventForm common semesters createForm =
    form [ onSubmit CreateEvent ]
        [ Basics.columns
            [ leftColumnInEventForm createForm
            , middleColumnInEventForm common semesters createForm
            , rightColumnInEventForm createForm
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
        [ textInput
            { title = "Event Name"
            , value = newEvent.name
            , placeholder = "Flashmobbing the HOMO SEX IS SIN people"
            , helpText = Just "Make it descriptive, make it short."
            , required = True
            , onInput = \name -> UpdateNewEventForm { newEvent | name = name }
            }
        , textInput
            { title = "Event Location"
            , value = newEvent.location
            , placeholder = "Your mom's house 😂"
            , helpText = Just "ha gottem"
            , required = False
            , onInput = \location -> UpdateNewEventForm { newEvent | location = location }
            }
        , dateInput
            { title = "Date of Event"
            , value = newEvent.callDate
            , placeholder = ""
            , helpText = Nothing
            , required = True
            , onInput = \callDate -> UpdateNewEventForm { newEvent | callDate = callDate }
            }
        , timeInput
            { title = "Call Time"
            , value = newEvent.callTime
            , placeholder = ""
            , helpText = Just "4:20 lamo"
            , required = True
            , onInput = \callTime -> UpdateNewEventForm { newEvent | callTime = callTime }
            }
        , timeInput
            { title = "Event Time"
            , value = newGig.performanceTime
            , placeholder = ""
            , helpText = Just "4:21 lamo"
            , required = False
            , onInput = \performanceTime -> UpdateNewGigForm { newGig | performanceTime = performanceTime }
            }
        , timeInput
            { title = "Release Time"
            , value = newEvent.releaseTime
            , placeholder = ""
            , helpText = Just "4:22 lamo"
            , required = False
            , onInput = \releaseTime -> UpdateNewEventForm { newEvent | releaseTime = releaseTime }
            }
        , dateInput
            { title = "Release Date"
            , value = newEvent.releaseDate
            , placeholder = ""
            , helpText = Nothing
            , required = False
            , onInput = \releaseDate -> UpdateNewEventForm { newEvent | releaseDate = releaseDate }
            }
        , textInput
            { title = "How many points is this worth?"
            , value = newEvent.points |> Maybe.map String.fromInt |> Maybe.withDefault ""
            , placeholder = "69"
            , helpText = Nothing
            , required = False
            , onInput = \points -> UpdateNewEventForm { newEvent | points = points |> String.toInt }
            }
        ]


middleColumnInEventForm : Common -> RemoteData (List Semester) -> CreateEventForm -> Html Msg
middleColumnInEventForm common remoteSemesters createForm =
    let
        newEvent =
            createForm.newEvent

        newGig =
            createForm.newGig

        eventTypeOption selectedType eventType =
            label [ class "radio" ]
                [ input
                    [ type_ "radio"
                    , checked <| eventType.name == selectedType
                    , onClick <| UpdateNewEventForm { newEvent | type_ = eventType.name }
                    ]
                    []
                , text eventType.name
                ]

        ( semesters, semestersAreLoading ) =
            case remoteSemesters of
                Loaded sems ->
                    ( sems |> List.map .name, "" )

                Loading ->
                    ( [ common.currentSemester.name ], " is-loading" )

                _ ->
                    ( [ common.currentSemester.name ], "" )
    in
    Basics.column
        [ fieldWrapper { title = "Event Type", helpText = Nothing } <|
            (common.info.eventTypes
                |> List.map (eventTypeOption newEvent.type_)
                |> List.intersperse (br [] [])
            )
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Semester" ]
            , div [ class <| "select control" ++ semestersAreLoading ]
                [ select
                    [ onInput (\semester -> UpdateNewEventForm { newEvent | semester = semester }) ]
                    (semesters |> List.map (\s -> option [ value s, selected (s == newEvent.semester) ] [ text s ]))
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Uniform" ]
            , div [ class "select control" ]
                [ select
                    [ onInput
                        (\uniform ->
                            UpdateNewGigForm
                                { newGig
                                    | uniform =
                                        common.info.uniforms
                                            |> List.find (\u -> u.name == uniform)
                                }
                        )
                    ]
                    (option
                        [ value "", selected <| isNothing newGig.uniform ]
                        [ text "(no uniform)" ]
                        :: (common.info.uniforms
                                |> List.map
                                    (\u ->
                                        option
                                            [ value u.name
                                            , selected
                                                (newGig.uniform
                                                    |> Maybe.map (\current -> current.id == u.id)
                                                    |> Maybe.withDefault False
                                                )
                                            ]
                                            [ text u.name ]
                                    )
                           )
                    )
                ]
            ]
        , fieldWrapper { title = "Event Summary", helpText = Nothing } <|
            [ textarea
                [ class "textarea"
                , placeholder "We're gonna get in there, we're gonna use our mouths, and we're gonna get out."
                , value newEvent.comments
                , onInput (\comments -> UpdateNewEventForm { newEvent | comments = comments })
                ]
                []
            ]
        ]


rightColumnInEventForm : CreateEventForm -> Html Msg
rightColumnInEventForm createForm =
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
                , textInput
                    { title = "Public Summary"
                    , value = newGig.summary
                    , placeholder = "Friends? Countrymen? Bueller?"
                    , helpText = Just "Careful, real people will see this"
                    , required = False
                    , onInput = \summary -> UpdateNewGigForm { newGig | summary = summary }
                    }
                , textareaInput
                    { title = "Public Description"
                    , value = newGig.description
                    , placeholder = "We the people, in order to kick a more perfect ass, I don;t know where this is going"
                    , helpText = Just "Careful, real people will see this"
                    , required = False
                    , onInput = \description -> UpdateNewGigForm { newGig | description = description }
                    }
                ]
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
               , div [ class "field" ]
                    [ label [ class "label" ] [ text "Repeat" ]
                    , div [ class "select control" ]
                        [ select
                            [ value <| periodToString newEvent.repeat
                            , onInput (\period -> UpdateNewEventForm { newEvent | repeat = stringToPeriod period })
                            ]
                            (allRepeatPeriods
                                |> List.map
                                    (\p ->
                                        option [ value <| periodToString p ] [ text <| (periodToString p |> capitalizeFirstChar) ]
                                    )
                            )
                        ]
                    ]
               , dateInput
                    { title = "Repeat Until"
                    , value = newEvent.repeatUntil
                    , placeholder = ""
                    , helpText = Nothing
                    , required = newEvent.repeat /= NoRepeat
                    , onInput = \repeatUntil -> UpdateNewEventForm { newEvent | repeatUntil = repeatUntil }
                    }
               , br [] []
               , button [ type_ "submit", class "button is-primary" ] [ text "Yeehaw" ]
               ]
        )
