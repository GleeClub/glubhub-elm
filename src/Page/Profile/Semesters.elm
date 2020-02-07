module Page.Profile.Semesters exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Models.Event exposing (ActiveSemester, Member, activeSemesterDecoder)
import Models.Info exposing (Enrollment(..), enrollmentToString)
import Request
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), resultToRemote, roundToTwoDigits)



---- MODEL ----


type alias Model =
    RemoteData (List ActiveSemester)


init : Common -> Member -> ( Model, Cmd Msg )
init common member =
    ( Loading, loadSemesters common member )



---- UPDATE ----


type Msg
    = OnLoadSemesters (GreaseResult (List ActiveSemester))


update : Msg -> Model -> ( Model, Cmd Msg )
update (OnLoadSemesters result) _ =
    ( resultToRemote result, Cmd.none )



---- DATA ----


loadSemesters : Common -> Member -> Cmd Msg
loadSemesters common member =
    let
        url =
            "/members/" ++ member.email ++ "?details=true"

        decoder =
            Decode.field "semesters" (Decode.list activeSemesterDecoder)
    in
    Request.get common url decoder
        |> Task.attempt OnLoadSemesters



---- VIEW ----


view : Model -> Html Msg
view model =
    model
        |> Basics.remoteContent profileSemesters


profileSemesters : List ActiveSemester -> Html Msg
profileSemesters semesters =
    let
        headerRow =
            tr []
                ([ "Semester", "Status", "Section", "Score" ]
                    |> List.map (\column -> th [] [ text column ])
                )

        semesterRow semester =
            tr [ class "no-bottom-border" ]
                (semesterRowValues semester
                    |> List.map (\cell -> td [] [ text cell ])
                )

        semesterRowValues semester =
            [ semester.semester
            , semester.enrollment
                |> Maybe.map enrollmentToString
                |> Maybe.withDefault "Inactive"
            , semester.section
                |> Maybe.withDefault "Homeless"
            , semester.grades.grade
                |> roundToTwoDigits
                |> String.fromFloat
            ]
    in
    table [ class "table" ]
        [ thead []
            [ headerRow ]
        , tbody []
            (semesters |> List.map semesterRow)
        ]
