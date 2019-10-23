module Components.NavBar exposing (confirmAccountHeader, navBar, navBarLinks, navBarLogoAndBurger)

import Html exposing (Html, a, button, div, form, h1, img, input, label, section, span, text)
import Html.Attributes exposing (attribute, class, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick)
import Maybe.Extra exposing (filter, isJust)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), spinner)


navBar : Maybe Common -> Bool -> Html Msg
navBar maybeCommon burgerOpened =
    nav [ class "navbar is-primary", role "navigation", attribute "aria-label" "main navigation" ]
        [ navBarLogoAndBurger burgerOpened
        , navBarLinks maybeCommon
        ]


navBarLogoAndBurger : Bool -> Html Msg
navBarLogoAndBurger burgerOpened =
    div [ class "navbar-brand" ]
        [ a [ Route.href Route.Home, class "navbar-item" ]
            [ span [ class "icon is-small", style "width" "3vw" ]
                [ i [ class "fas fa-home" ] [] ]
            ]
        , a
            [ role "button"
            , class "navbar-burger"
                ++ (if burgerOpened then
                        " is-active"

                    else
                        ""
                   )
            , attribute "aria-label" "menu"
            , attribute "aria-expanded" "false"
            , onClick ToggleBurger
            ]
            [ span [ attribute "aria-hidden" "true" ]
            , span [ attribute "aria-hidden" "true" ]
            , span [ attribute "aria-hidden" "true" ]
            ]
        ]


navBarLinks : Maybe Common -> Html msg
navBarLinks maybeCommon =
    let
        ( primaryLinks, profileLink ) =
            case maybeCommon of
                Just common ->
                    ( [ a [ Route.href Route.Event { id = Nothing, tab = Nothing } ] [ text "Events" ]
                      , a [ Route.href Route.Repertoire Nothing ] [ text "Music" ]
                      , a [ Route.href Route.Roster ] [ text "People" ]
                      , a [ Route.href Route.Minutes { id = Nothing, tab = Nothing } ] [ text "Minutes" ]
                      , a [ Route.href Route.Officer { tab = Nothing } ] [ text "Admin" ]
                      ]
                    , [ a
                            [ Route.href Route.Profile common.user.email
                            , class "navbar-item"
                            ]
                            [ text common.user.fullName ]
                      ]
                    )

                Nothing ->
                    ( [], [] )
    in
    div [ class "navbar-menu" ]
        [ div [ class "navbar-start" ] primaryLinks
        , div [ class "navbar-end" ] profileLink
        ]


confirmAccountHeader : Bool -> Html Msg
confirmAccountHeader memberIsActive =
    if memberIsActive then
        section [ style "display" "none" ] []

    else
        section [ style "margin" "2em", style "margin-bottom" "-1em" ]
            [ div [ class "notification is-info" ]
                [ button [ class "delete", onClick IgnoreConfirmPrompt ]
                , div
                    [ style "width" "100%"
                    , style "display" "flex"
                    , style "align-items" "center"
                    ]
                    [ div []
                        [ text "Welcome! Feel free to browse the site, but"
                        , text "if you're going to be active in Glee Club this"
                        , text "semester, please confirm your account so we"
                        , text "can get you into the system."
                        ]
                    , div []
                        [ a
                            [ Route.href Route.ConfirmAccount
                            , style "margin" "0 2em"
                            , class "button is-info is-inverted is-outlined"
                            ]
                        ]
                        [ text "Confirm" ]
                    ]
                ]
            ]
