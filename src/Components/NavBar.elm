module Components.NavBar exposing (NavBar, navBar)

import Html exposing (Html, a, div, i, nav, span, text)
import Html.Attributes exposing (attribute, class, href, style, target)
import Html.Events exposing (onClick)
import Maybe.Extra exposing (isJust)
import Route exposing (Route(..))
import Utils exposing (Common, RemoteData(..), fullName)


type alias NavBar msg =
    { common : Maybe Common
    , burgerOpened : Bool
    , toggleBurger : msg
    }


navBar : NavBar msg -> Html msg
navBar data =
    nav
        [ class "navbar is-primary is-fixed-top"
        , attribute "role" "navigation"
        , attribute "aria-label" "main navigation"
        ]
        [ navBarLogoAndBurger data
        , navBarLinks data
        ]


navBarLogoAndBurger : NavBar msg -> Html msg
navBarLogoAndBurger data =
    let
        homeLogo =
            a [ Route.href Route.Home, class "navbar-item" ]
                [ span [ class "icon is-small", style "width" "3vw" ]
                    [ i [ class "fas fa-home" ] [] ]
                ]

        burgerButton =
            a
                [ attribute "role" "button"
                , class <|
                    "navbar-burger"
                        ++ (if data.burgerOpened then
                                " is-active"

                            else
                                ""
                           )
                , attribute "aria-label" "menu"
                , attribute "aria-expanded" <|
                    if data.burgerOpened then
                        "true"

                    else
                        "false"
                , onClick data.toggleBurger
                ]
                [ span [ attribute "aria-hidden" "true" ] []
                , span [ attribute "aria-hidden" "true" ] []
                , span [ attribute "aria-hidden" "true" ] []
                ]
    in
    div [ class "navbar-brand" ]
        [ homeLogo
        , if isJust (data.common |> Maybe.andThen .user) then
            burgerButton

          else
            text ""
        ]


navBarLinks : NavBar msg -> Html msg
navBarLinks data =
    let
        singleLink title route =
            a [ class "navbar-item", Route.href route ] [ text title ]

        ( primaryLinks, profileLink ) =
            case data.common |> Maybe.andThen .user of
                Just user ->
                    ( [ singleLink "Events" <| Route.Events { id = Nothing, tab = Nothing }
                      , singleLink "Music" <| Route.Repertoire Nothing
                      , singleLink "People" Route.Roster
                      , singleLink "Minutes" <| Route.Minutes { id = Nothing, tab = Nothing }
                      , documentLinks data.common
                      ]
                        ++ (if List.isEmpty user.permissions then
                                []

                            else
                                [ singleLink "Admin" <| Route.Admin Nothing ]
                           )
                    , [ singleLink (user |> fullName) <| Route.Profile user.email ]
                    )

                Nothing ->
                    ( [], [] )
    in
    div
        [ class <|
            "navbar-menu"
                ++ (if data.burgerOpened then
                        " is-active"

                    else
                        ""
                   )
        ]
        [ div [ class "navbar-start" ] primaryLinks
        , div [ class "navbar-end" ] profileLink
        ]


documentLinks : Maybe Common -> Html msg
documentLinks common =
    let
        allDocuments =
            common
                |> Maybe.map (.info >> .documents)
                |> Maybe.withDefault []

        documentLink document =
            a
                [ class "navbar-item", target "_blank", href document.url ]
                [ text document.name ]
    in
    div
        [ class "navbar-item has-dropdown is-hoverable" ]
        [ a [ class "navbar-link" ] [ text "Documents" ]
        , div [ class "navbar-dropdown" ]
            (allDocuments |> List.map documentLink)
        ]
