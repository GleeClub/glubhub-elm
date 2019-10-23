port module Utils exposing (Common, RemoteData(..), alert, apiUrl, formatPhone, getRequest, handleJsonResponse, notFoundView, permittedTo, postRequest, romanNumeral, scrollToElement, setToken, spinner)

import Browser.Navigation as Nav
import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder)
import Models.Info exposing (Info, Semester)
import Models.Member exposing (Member)



---- CONSTANTS ----


apiUrl =
    "https://gleeclub.gatech.edu/cgi-bin/api"



---- REUSED COMPONENTS ----


spinner : Html a
spinner =
    div [ class "spinner" ]
        [ div [ class "spinner-inner" ]
            [ i [ class "oldgold-text fas fa-circle-notch fa-2x fa-spin" ] []
            ]
        ]


notFoundView : Html a
notFoundView =
    div []
        [ text "Not found"
        ]



---- TYPES ----


type RemoteData a
    = NotAsked
    | Loading
    | Loaded a
    | Failure


type alias Common =
    { user : Member
    , members : List Member
    , info : Info
    , currentSemester : Semester
    , token : String
    , key : Nav.Key
    }



---- FUNCTIONS ----


getRequest : Common -> String -> Http.Expect msg -> Cmd msg
getRequest common url expect =
    Http.request
        { method = "GET"
        , url = apiUrl ++ url
        , body = Http.emptyBody
        , headers = [ Http.header "token" common.token ]
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


postRequest : Common -> String -> Http.Body -> (Result Http.Error () -> msg) -> Cmd msg
postRequest common url body expect =
    Http.request
        { method = "GET"
        , url = apiUrl ++ url
        , body = Http.emptyBody
        , headers = [ Http.header "token" common.token ]
        , expect = Http.expectWhatever expect
        , timeout = Nothing
        , tracker = Nothing
        }


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
    user.permissions |> List.any (\p -> p == permission)


port setToken : Maybe String -> Cmd msg


port alert : String -> Cmd msg


port scrollToElement : String -> Cmd msg
