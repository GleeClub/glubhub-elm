port module Utils exposing
    ( Common
    , RemoteData(..)
    , SubmissionState(..)
    , alert
    , apiUrl
    , checkSubmissionResult
    , decodeId
    , deleteRequest
    , deployEditor
    , eventIsOver
    , formatPhone
    , fullName
    , getRequest
    , goldColor
    , handleJsonResponse
    , isActiveClass
    , isLoadingClass
    , isPrimaryClass
    , mapLoaded
    , optionalSingleton
    , permittedTo
    , playPitch
    , postRequest
    , postRequestFull
    , rawHtml
    , remoteToMaybe
    , resultToRemote
    , resultToSubmissionState
    , romanNumeral
    , scrollToElement
    , setToken
    , showIfPermittedTo
    , submissionStateBoxId
    , timeout
    )

import Browser.Navigation as Nav
import Color exposing (Color)
import Error exposing (GreaseError, parseResponse)
import Html exposing (Html)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe.Extra
import Models.Event exposing (HasCallAndReleaseTimes, Member)
import Models.Info exposing (Info, Semester)
import Task exposing (Task)
import Time exposing (Posix, Zone, posixToMillis)



---- CONSTANTS ----


apiUrl : String
apiUrl =
    "https://gleeclub.gatech.edu/cgi-bin/api"


goldColor : Color
goldColor =
    Color.rgb 0.706 0.643 0.412


timeout : Maybe Float
timeout =
    Just (1000 * 20)


submissionStateBoxId : String
submissionStateBoxId =
    "submission-state-box"



---- REUSED COMPONENTS ----


rawHtml : String -> List (Html msg)
rawHtml content =
    case Html.Parser.run content of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []



---- TYPES ----


type RemoteData a
    = NotAsked
    | Loading
    | Loaded a
    | Failure GreaseError


type alias Common =
    { user : Maybe Member
    , members : List Member
    , info : Info
    , currentSemester : Semester
    , token : String
    , key : Nav.Key
    , timeZone : Zone
    , now : Posix
    }


type SubmissionState
    = NotSentYet
    | Sending
    | ErrorSending GreaseError



---- FUNCTIONS ----


optionalSingleton : Bool -> Html msg -> List (Html msg)
optionalSingleton shouldRender content =
    if shouldRender then
        [ content ]

    else
        []


mapLoaded : (a -> b) -> RemoteData a -> RemoteData b
mapLoaded mapper remoteData =
    case remoteData of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Loaded data ->
            Loaded (mapper data)

        Failure err ->
            Failure err


remoteToMaybe : RemoteData a -> Maybe a
remoteToMaybe remoteData =
    case remoteData of
        Loaded data ->
            Just data

        _ ->
            Nothing


resultToRemote : Result GreaseError a -> RemoteData a
resultToRemote result =
    case result of
        Ok success ->
            Loaded success

        Err error ->
            Failure error


resultToSubmissionState : Result GreaseError a -> SubmissionState
resultToSubmissionState result =
    case result of
        Ok _ ->
            NotSentYet

        Err error ->
            ErrorSending error


checkSubmissionResult :
    { a | state : SubmissionState }
    -> Result GreaseError ()
    -> ( { a | state : SubmissionState }, Cmd msg )
checkSubmissionResult model result =
    case result of
        Ok _ ->
            ( { model | state = NotSentYet }, Cmd.none )

        Err error ->
            ( { model | state = ErrorSending error }
            , scrollToElement submissionStateBoxId
            )


getRequest : { a | token : String } -> String -> Decoder b -> Task GreaseError b
getRequest common url decoder =
    Http.task
        { method = "GET"
        , url = apiUrl ++ url
        , body = Http.emptyBody
        , headers = [ Http.header "token" common.token ]
        , resolver = Http.stringResolver <| parseResponse decoder
        , timeout = timeout
        }


postRequestFull : { a | token : String } -> String -> Encode.Value -> Decoder b -> Task GreaseError b
postRequestFull common url body decoder =
    Http.task
        { method = "POST"
        , url = apiUrl ++ url
        , body = Http.jsonBody body
        , headers = [ Http.header "token" common.token ]
        , resolver = Http.stringResolver <| parseResponse decoder
        , timeout = timeout
        }


postRequest : { a | token : String } -> String -> Encode.Value -> Task GreaseError ()
postRequest common url body =
    postRequestFull common url body (Decode.succeed ())


deleteRequest : { a | token : String } -> String -> Task GreaseError ()
deleteRequest common url =
    Http.task
        { method = "DELETE"
        , url = apiUrl ++ url
        , body = Http.emptyBody
        , headers = [ Http.header "token" common.token ]
        , resolver = Http.stringResolver <| parseResponse (Decode.succeed ())
        , timeout = timeout
        }


decodeId : Decoder Int
decodeId =
    Decode.field "id" Decode.int


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


eventIsOver : Posix -> HasCallAndReleaseTimes a -> Bool
eventIsOver now { callTime, releaseTime } =
    posixToMillis now > posixToMillis (releaseTime |> Maybe.withDefault callTime)


formatPhone : String -> String
formatPhone phone =
    if String.length phone == 10 then
        "(" ++ String.slice 0 3 phone ++ ") " ++ String.slice 3 6 phone ++ "-" ++ String.slice 6 10 phone

    else
        phone


romanNumeral : Int -> String
romanNumeral n =
    let
        numbers =
            [ ( "zero", "0" )
            , ( "one", "I" )
            , ( "two", "II" )
            , ( "three", "III" )
            , ( "four", "IV" )
            , ( "five", "V" )
            , ( "six", "VI" )
            , ( "seven", "VII" )
            , ( "eight", "VIII" )
            , ( "nine", "IX" )
            , ( "ten", "X" )
            , ( "eleven", "XI" )
            , ( "twelve", "XII" )
            , ( "thirteen", "XIII" )
            , ( "fourteen", "XIV" )
            , ( "fifteen", "XV" )
            , ( "sixteen", "XVI" )
            , ( "seventeen", "XVII" )
            , ( "eighteen", "XVIII" )
            , ( "nineteen", "XIX" )
            , ( "twenty", "XX" )
            ]
    in
    case numbers |> List.drop n |> List.head of
        Just ( number, numeral ) ->
            number ++ " (" ++ numeral ++ ")"

        Nothing ->
            String.fromInt n


permittedTo : String -> Member -> Bool
permittedTo permission user =
    user.permissions |> List.any (\p -> p.name == permission)


showIfPermittedTo : String -> Member -> Html msg -> List (Html msg)
showIfPermittedTo permission user content =
    optionalSingleton (permittedTo permission user) content


fullName : Member -> String
fullName member =
    let
        firstName =
            member.preferredName
                |> Maybe.Extra.filter (\n -> String.length n > 0)
                |> Maybe.withDefault member.firstName
    in
    firstName ++ " " ++ member.lastName


isLoadingClass : Bool -> String
isLoadingClass isLoading =
    if isLoading then
        " is-loading"

    else
        ""


isActiveClass : Bool -> String
isActiveClass isActive =
    if isActive then
        " is-active"

    else
        ""


isPrimaryClass : Bool -> String
isPrimaryClass isPrimary =
    if isPrimary then
        " is-primary"

    else
        ""



-- PORTS --


port setToken : Maybe String -> Cmd msg


port alert : String -> Cmd msg


port scrollToElement : String -> Cmd msg


port playPitch : Int -> Cmd msg


port deployEditor : { elementId : String, content : String } -> Cmd msg
