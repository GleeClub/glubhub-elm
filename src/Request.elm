module Request exposing (delete, get, post, postFull, postReturningId)

import Error exposing (GreaseError, parseResponse)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)


apiUrl : String
apiUrl =
    "https://gleeclub.gatech.edu/cgi-bin/api"


timeout : Maybe Float
timeout =
    Just (1000 * 20)


get : { a | token : String } -> String -> Decoder b -> Task GreaseError b
get { token } url decoder =
    Http.task
        { method = "GET"
        , url = apiUrl ++ url
        , body = Http.emptyBody
        , headers = [ Http.header "token" token ]
        , resolver = Http.stringResolver <| parseResponse decoder
        , timeout = timeout
        }


postFull : { a | token : String } -> String -> Encode.Value -> Decoder b -> Task GreaseError b
postFull { token } url body decoder =
    Http.task
        { method = "POST"
        , url = apiUrl ++ url
        , body = Http.jsonBody body
        , headers = [ Http.header "token" token ]
        , resolver = Http.stringResolver <| parseResponse decoder
        , timeout = timeout
        }


post : { a | token : String } -> String -> Encode.Value -> Task GreaseError ()
post common url body =
    postFull common url body (Decode.succeed ())


postReturningId : { a | token : String } -> String -> Encode.Value -> Task GreaseError Int
postReturningId common url body =
    postFull common url body (Decode.field "id" Decode.int)


delete : { a | token : String } -> String -> Task GreaseError ()
delete { token } url =
    Http.task
        { method = "DELETE"
        , url = apiUrl ++ url
        , body = Http.emptyBody
        , headers = [ Http.header "token" token ]
        , resolver = Http.stringResolver <| parseResponse (Decode.succeed ())
        , timeout = timeout
        }
