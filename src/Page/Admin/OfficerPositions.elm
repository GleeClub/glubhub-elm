module Page.Admin.OfficerPositions exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, div, option, select, span, table, td, text, tr)
import Html.Attributes exposing (class, selected, style, value)
import Html.Events exposing (onInput)
import Json.Decode as Decode exposing (string)
import Json.Encode as Encode
import List.Extra exposing (find)
import Maybe.Extra exposing (isNothing)
import Models.Event exposing (Member, MemberRole, memberRoleDecoder)
import Models.Info exposing (Role)
import Route exposing (AdminTab(..))
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), fullName, getRequest, mapLoaded, postRequest, resultToRemote, resultToSubmissionState)



---- MODEL ----


type alias Model =
    { common : Common
    , roles : RemoteData (List MemberRole)
    , state : SubmissionState
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common
      , roles = Loading
      , state = NotSentYet
      }
    , loadCurrentOfficers common
    )



---- UPDATE ----


type alias ToggleOfficerData =
    { role : Role
    , oldMember : Maybe Member
    , newMember : Maybe Member
    }


type Msg
    = OnLoadOfficerPositions (GreaseResult (List MemberRole))
    | ToggleOfficer ToggleOfficerData
    | OnChange (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadOfficerPositions rolesResult ->
            ( { model | roles = resultToRemote rolesResult }, Cmd.none )

        OnChange result ->
            ( { model | state = resultToSubmissionState result }, Cmd.none )

        ToggleOfficer data ->
            case ( data.oldMember, data.newMember ) of
                ( Nothing, Nothing ) ->
                    ( model, Cmd.none )

                ( Just oldMember, Nothing ) ->
                    removeMember model { role = data.role, member = oldMember }

                ( Nothing, Just newMember ) ->
                    addMember model { role = data.role, member = newMember }

                ( Just oldMember, Just newMember ) ->
                    switchMembers model data.role ( oldMember, newMember )


removeMember : Model -> MemberRole -> ( Model, Cmd Msg )
removeMember model memberRole =
    let
        memberRoleFilter r =
            (r.role.name /= memberRole.role.name) || (r.member.email /= memberRole.member.email)
    in
    ( { model
        | roles = model.roles |> mapLoaded (List.filter memberRoleFilter)
        , state = Sending
      }
    , removeMemberRole model.common memberRole
    )


addMember : Model -> MemberRole -> ( Model, Cmd Msg )
addMember model memberRole =
    ( { model
        | roles = model.roles |> mapLoaded (\roles -> roles ++ [ memberRole ])
        , state = Sending
      }
    , addMemberRole model.common memberRole
    )


switchMembers : Model -> Role -> ( Member, Member ) -> ( Model, Cmd Msg )
switchMembers model role ( oldMember, newMember ) =
    if oldMember.email == newMember.email then
        ( model, Cmd.none )

    else
        let
            memberRole =
                { role = role, member = newMember }

            memberRoleMapper r =
                if r.role.name == role.name && r.member.email == oldMember.email then
                    memberRole

                else
                    r
        in
        ( { model
            | roles = model.roles |> mapLoaded (List.map memberRoleMapper)
            , state = Sending
          }
        , switchMemberRole model.common role ( oldMember, newMember )
        )



---- DATA ----


loadCurrentOfficers : Common -> Cmd Msg
loadCurrentOfficers common =
    getRequest common "/member_roles" (Decode.list memberRoleDecoder)
        |> Task.attempt OnLoadOfficerPositions


addMemberRole : Common -> MemberRole -> Cmd Msg
addMemberRole common memberRole =
    postRequest common "/roles/add" (serializeMemberRole memberRole)
        |> Task.attempt OnChange


removeMemberRole : Common -> MemberRole -> Cmd Msg
removeMemberRole common memberRole =
    postRequest common "/roles/remove" (serializeMemberRole memberRole)
        |> Task.attempt OnChange



-- TODO: fix loading issue


switchMemberRole : Common -> Role -> ( Member, Member ) -> Cmd Msg
switchMemberRole common role ( oldMember, newMember ) =
    let
        removeTask =
            postRequest common "/roles/remove" (serializeMemberRole { role = role, member = oldMember })

        addTask _ =
            postRequest common "/roles/add" (serializeMemberRole { role = role, member = newMember })
    in
    removeTask
        |> Task.andThen addTask
        |> Task.attempt OnChange


serializeMemberRole : MemberRole -> Encode.Value
serializeMemberRole memberRole =
    Encode.object
        [ ( "member", Encode.string memberRole.member.email )
        , ( "role", Encode.string memberRole.role.name )
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Positions"
        , Basics.box
            [ model.roles |> Basics.remoteContent (allRoleRows model.common)
            , Basics.submissionStateBox model.state
            ]
        ]


allRoleRows : Common -> List MemberRole -> Html Msg
allRoleRows common memberRoles =
    table [ style "border-spacing" "5px", style "border-collapse" "separate" ]
        (common.info.roles
            |> List.sortBy .rank
            |> List.concatMap (roleRows common.members memberRoles)
        )


roleRows : List Member -> List MemberRole -> Role -> List (Html Msg)
roleRows allMembers memberRoles role =
    let
        membersWithRole =
            memberRoles
                |> List.filter (\mr -> mr.role.name == role.name)
                |> List.map .member

        displayCount =
            min role.maxQuantity (List.length membersWithRole + 1)
    in
    membersWithRole
        |> List.map (\m -> Just m)
        |> (\members -> members ++ [ Nothing ])
        |> List.take displayCount
        |> List.map (memberDropdown role allMembers)


memberDropdown : Role -> List Member -> Maybe Member -> Html Msg
memberDropdown role allMembers selectedMember =
    tr []
        [ td [ style "padding-right" "10px" ]
            [ span [ style "display" "inline-block", style "vertical-align" "middle" ]
                [ text <| role.name ]
            ]
        , td []
            [ div [ class "select" ]
                [ select
                    [ onInput
                        (\email ->
                            ToggleOfficer
                                { role = role
                                , oldMember = selectedMember
                                , newMember = allMembers |> find (\m -> m.email == email)
                                }
                        )
                    ]
                    (option [ value "", selected (isNothing selectedMember) ] [ text "(nobody)" ]
                        :: (allMembers
                                |> List.map
                                    (\m ->
                                        option
                                            [ value m.email
                                            , selected
                                                (selectedMember
                                                    |> Maybe.map (\s -> s.email == m.email)
                                                    |> Maybe.withDefault False
                                                )
                                            ]
                                            [ text (m |> fullName) ]
                                    )
                           )
                    )
                ]
            ]
        ]
