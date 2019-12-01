module Page.Admin.MakeAnnouncement exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, a, b, br, button, div, h1, i, img, input, label, p, section, span, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (class, id, style, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import Route exposing (AdminTab(..), Route)
import Task
import Utils exposing (Common, SubmissionState(..), postRequest)



---- MODEL ----


type alias Model =
    { common : Common
    , content : String
    , state : SubmissionState
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , content = ""
      , state = NotSentYet
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateAnnouncement String
    | Submit
    | OnSubmitAnnouncement (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAnnouncement content ->
            ( { model | content = content }, Cmd.none )

        Submit ->
            ( { model | state = Sending }, makeAnnouncement model.common model.content )

        OnSubmitAnnouncement (Ok _) ->
            ( { model | state = NotSentYet }, Route.loadPage <| Route.Admin (Just AdminAnnouncements) )

        OnSubmitAnnouncement (Err error) ->
            ( { model | state = ErrorSending error }, Cmd.none )



---- DATA ----


makeAnnouncement : Common -> String -> Cmd Msg
makeAnnouncement common content =
    let
        json =
            Encode.object [ ( "content", Encode.string content ) ]
    in
    postRequest common "/announcements" json
        |> Task.attempt OnSubmitAnnouncement



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ style "min-width" "50%" ]
        [ Basics.title "Make an Announcement"
        , Basics.box
            [ p [ class "is-5" ] [ text "Your words shall ring through the halls of Valhalla!" ]
            , div [ style "padding-top" "10px", style "padding-bottom" "10px" ]
                [ textarea
                    [ class "textarea"
                    , onInput UpdateAnnouncement
                    , value model.content
                    ]
                    []
                ]
            , submitButton model.state
            , case model.state of
                ErrorSending error ->
                    Basics.errorBox error

                _ ->
                    text ""
            ]
        ]


submitButton : SubmissionState -> Html Msg
submitButton state =
    button
        [ class "button is-primary"
        , class <|
            case state of
                Sending ->
                    "is-loading"

                _ ->
                    ""
        , onClick Submit
        ]
        [ text "Suck it through the tubes." ]
