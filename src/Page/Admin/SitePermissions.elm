module Page.Admin.SitePermissions exposing (Model, Msg(..), init, update, view)

import Components.Basics as Basics
import Error exposing (GreaseResult)
import Html exposing (Html, div, input, table, tbody, td, text, th, tr)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)
import Json.Decode as Decode exposing (string)
import Json.Encode as Encode
import List.Extra exposing (find)
import Maybe.Extra exposing (isJust)
import Models.Admin exposing (RolePermission, rolePermissionDecoder)
import Models.Info exposing (Permission, PermissionType(..))
import Route exposing (AdminTab(..))
import Task
import Utils exposing (Common, RemoteData(..), SubmissionState(..), checkSubmissionResult, getRequest, postRequest, resultToRemote)



---- MODEL ----


type alias Model =
    { common : Common
    , permissions : RemoteData (List RolePermission)
    , state : SubmissionState
    }


init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common, permissions = Loading, state = NotSentYet }, loadPermissions common )



---- UPDATE ----


type Msg
    = OnLoadPermissions (GreaseResult (List RolePermission))
    | TogglePermission RolePermission
    | OnTogglePermission (GreaseResult ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadPermissions result ->
            ( { model | permissions = resultToRemote result }, Cmd.none )

        OnTogglePermission result ->
            checkSubmissionResult model result

        TogglePermission permission ->
            case model.permissions of
                Loaded permissions ->
                    if permissions |> find (permissionsAreEqual permission) |> isJust then
                        ( { model
                            | permissions =
                                Loaded
                                    (permissions
                                        |> List.filter (permissionsAreEqual permission >> not)
                                    )
                            , state = Sending
                          }
                        , disablePermission model.common permission
                        )

                    else
                        ( { model
                            | permissions = Loaded (permissions ++ [ permission ])
                            , state = Sending
                          }
                        , enablePermission model.common permission
                        )

                _ ->
                    ( model, Cmd.none )



---- DATA ----


permissionsAreEqual : RolePermission -> RolePermission -> Bool
permissionsAreEqual permission1 permission2 =
    (permission1.role == permission2.role)
        && (permission1.permission == permission2.permission)
        && ((permission1.eventType |> Maybe.withDefault "") == (permission2.eventType |> Maybe.withDefault ""))


loadPermissions : Common -> Cmd Msg
loadPermissions common =
    getRequest common "/role_permissions" (Decode.list rolePermissionDecoder)
        |> Task.attempt OnLoadPermissions


disablePermission : Common -> RolePermission -> Cmd Msg
disablePermission common rolePermission =
    let
        url =
            "/permissions/" ++ rolePermission.role ++ "/disable"

        permission =
            serializePermission rolePermission
    in
    postRequest common url permission
        |> Task.attempt OnTogglePermission


enablePermission : Common -> RolePermission -> Cmd Msg
enablePermission common rolePermission =
    let
        url =
            "/permissions/" ++ rolePermission.role ++ "/enable"

        permission =
            serializePermission rolePermission
    in
    postRequest common url permission
        |> Task.attempt OnTogglePermission


serializePermission : RolePermission -> Encode.Value
serializePermission rolePermission =
    Encode.object
        [ ( "name", Encode.string rolePermission.permission )
        , ( "eventType"
          , rolePermission.eventType
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        ]


permissionEventTypes : Common -> Permission -> List (Maybe String)
permissionEventTypes common permission =
    case permission.type_ of
        StaticPermission ->
            [ Nothing ]

        EventPermission ->
            Nothing :: (common.info.eventTypes |> List.map (\type_ -> Just type_.name))



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Basics.title "Permissions"
        , Basics.box
            [ model.permissions |> Basics.remoteContent (permissionsGrid model.common)
            , Basics.submissionStateBox model.state
            ]
        ]


permissionsGrid : Common -> List RolePermission -> Html Msg
permissionsGrid common currentPermissions =
    table []
        [ tbody []
            (rolesHeaderRow common
                :: (common.info.permissions
                        |> List.concatMap (permissionRows common currentPermissions)
                   )
            )
        ]


rolesHeaderRow : Common -> Html Msg
rolesHeaderRow common =
    let
        roleCell role =
            th [ class "vertheader" ]
                [ div [] [ text role.name ] ]
    in
    tr [] (th [] [] :: (common.info.roles |> List.map roleCell))


permissionRows : Common -> List RolePermission -> Permission -> List (Html Msg)
permissionRows common currentPermissions permission =
    permission
        |> permissionEventTypes common
        |> List.map (permissionRow common currentPermissions permission)


permissionRow : Common -> List RolePermission -> Permission -> Maybe String -> Html Msg
permissionRow common currentPermissions permission eventType =
    tr []
        (permissionName permission eventType
            :: (common.info.roles
                    |> List.map
                        (\role ->
                            permissionCheckbox currentPermissions
                                { role = role.name
                                , permission = permission.name
                                , eventType = eventType
                                }
                        )
               )
        )


permissionName : Permission -> Maybe String -> Html Msg
permissionName p eventType =
    td [ style "white-space" "nowrap" ]
        [ text <|
            p.name
                ++ (eventType
                        |> Maybe.map (\t -> ":" ++ t)
                        |> Maybe.withDefault ""
                   )
        ]


permissionCheckbox : List RolePermission -> RolePermission -> Html Msg
permissionCheckbox currentPermissions rolePermission =
    td (Basics.tooltip rolePermission.role)
        [ input
            [ type_ "checkbox"
            , onCheck (\_ -> TogglePermission rolePermission)
            , checked
                (currentPermissions
                    |> List.any (permissionsAreEqual rolePermission)
                )
            ]
            []
        ]
