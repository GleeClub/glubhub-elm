module Error exposing (GreaseError(..), GreaseResult, parseResponse)

import Http exposing (Response)
import Json.Decode as Decode exposing (Decoder, decodeString, field, maybe, oneOf, string)
import Models.Event exposing (Member, memberDecoder)


type alias GreaseResult a =
    Result GreaseError a


type GreaseError
    = Unauthorized
    | NotActiveYet Member
    | AlreadyLoggedIn String
    | Forbidden (Maybe String)
    | NotFound
    | BadRequest String
    | ServerError String
    | DatabaseError String
    | FromRowError String
    | UnknownError ( Maybe Int, String )


checkValue : a -> Decoder a -> Decoder ()
checkValue x decoder =
    decoder
        |> Decode.andThen
            (\val ->
                if val == x then
                    Decode.succeed ()

                else
                    Decode.fail "wrong value"
            )


checkString : String -> Decoder ()
checkString x =
    checkValue x string


greaseErrorDecoder : Decoder GreaseError
greaseErrorDecoder =
    oneOf
        [ Decode.map (\_ -> Unauthorized)
            (field "message" <| checkString "login required")
        , Decode.map2 (\_ member -> NotActiveYet member)
            (field "message" <| checkString "member not active yet")
            (field "member" memberDecoder)
        , Decode.map2 (\_ token -> AlreadyLoggedIn token)
            (field "message" <| checkString "member already logged in")
            (field "token" string)
        , Decode.map2 (\_ permission -> Forbidden permission)
            (field "message" <| checkString "access forbidden")
            (maybe (field "requiredPermission" string))
        , Decode.map (\_ -> NotFound)
            (field "message" <| checkString "resource not found")
        , Decode.map2 (\_ reason -> BadRequest reason)
            (field "message" <| checkString "bad request")
            (field "reason" string)
        , Decode.map2 (\_ err -> ServerError err)
            (field "message" <| checkString "server error")
            (field "error" string)
        , Decode.map2 (\_ err -> DatabaseError err)
            (field "message" <| checkString "database error")
            (field "error" string)
        , Decode.map2 (\_ err -> FromRowError err)
            (field "message" <| checkString "database error (error deserializing from returned row)")
            (field "error" string)
        ]


parseResponse : Decoder a -> Response String -> Result GreaseError a
parseResponse decoder response =
    case response of
        Http.BadUrl_ _ ->
            Err (UnknownError ( Nothing, "bad url" ))

        Http.Timeout_ ->
            Err (UnknownError ( Nothing, "timeout" ))

        Http.NetworkError_ ->
            Err (UnknownError ( Nothing, "network error" ))

        Http.BadStatus_ metadata body ->
            Err
                ((if String.isEmpty body then
                    "\"\""

                  else
                    body
                 )
                    |> decodeString greaseErrorDecoder
                    |> Result.withDefault (UnknownError ( Just metadata.statusCode, body ))
                )

        Http.GoodStatus_ _ body ->
            (if String.isEmpty body then
                "\"\""

             else
                body
            )
                |> decodeString decoder
                |> Result.mapError
                    (\decodeError ->
                        UnknownError ( Nothing, Decode.errorToString decodeError )
                    )
