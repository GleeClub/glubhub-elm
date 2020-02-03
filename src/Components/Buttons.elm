module Components.Buttons exposing
    ( ButtonAttribute(..)
    , ButtonColor(..)
    , ButtonSize(..)
    , GroupAlignment(..)
    , back
    , button
    , delete
    , group
    , link
    , submit
    )

import Color as ColorExt
import Error exposing (GreaseError(..))
import Html exposing (Html, a, div, i, span, text)
import Html.Attributes exposing (attribute, class, disabled, href, style, type_)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Route exposing (Route)


type alias BaseButton msg a =
    { a
        | content : String
        , attrs : List (ButtonAttribute msg)
    }


type alias BackButton msg =
    { content : String
    , onClick : msg
    }


back : BackButton msg -> Html msg
back data =
    span
        [ style "display" "inline-block"
        , style "vertical-align" "middle"
        , style "cursor" "pointer"
        , onClick data.onClick
        ]
        [ i
            [ style "display" "inline-block"
            , style "vertical-align" "middle"
            , class "fas fa-arrow-left"
            , style "font-size" "16px"
            ]
            []
        , text " "
        , span
            [ style "display" "inline-block"
            , style "vertical-align" "middle"
            ]
            [ text data.content ]
        ]


type alias Button msg =
    BaseButton msg { onClick : Maybe msg }


button : Button msg -> Html msg
button data =
    let
        fullClass =
            buttonClass data.attrs
                |> String.join " "

        otherAttrs =
            otherButtonAttrs data.attrs

        element =
            buttonElement data.attrs

        click =
            data.onClick
                |> Maybe.map onClick
                |> Maybe.withDefault (disabled True)
    in
    element (class fullClass :: click :: otherAttrs)
        [ text data.content ]


type alias LinkButton msg =
    BaseButton msg { route : Route }


submit : BaseButton msg {} -> Html msg
submit data =
    let
        linkClass =
            buttonClass data.attrs
                |> String.join " "

        otherAttrs =
            otherButtonAttrs data.attrs
    in
    Html.button (class linkClass :: type_ "submit" :: otherAttrs)
        [ text data.content ]


link : LinkButton msg -> Html msg
link data =
    let
        linkClass =
            buttonClass data.attrs
                |> String.join " "

        otherAttrs =
            otherButtonAttrs data.attrs
    in
    a (class linkClass :: Route.href data.route :: otherAttrs)
        [ text data.content ]


delete : msg -> Html msg
delete message =
    Html.button
        [ class "delete"
        , attribute "aria-label" "close"
        , onClick message
        ]
        []


type ButtonAttribute msg
    = Color ButtonColor
    | Size ButtonSize
    | IsLoading Bool
    | IsInverted
    | IsOutlined
    | Italic
    | CustomAttrs (List (Html.Attribute msg))
    | CustomElement (HtmlElement msg)


type alias HtmlElement msg =
    List (Html.Attribute msg) -> List (Html msg) -> Html msg


type ButtonColor
    = IsPrimary
    | IsDanger
    | IsInfo
    | CustomColor ColorExt.Color


type ButtonSize
    = Small
    | Normal
    | Medium
    | Large


buttonClass : List (ButtonAttribute msg) -> List String
buttonClass attrs =
    let
        isLoading =
            Just "is-loading"
                |> Maybe.filter (\_ -> attrs |> List.member (IsLoading True))

        isInverted =
            Just "is-inverted"
                |> Maybe.filter (\_ -> attrs |> List.member IsInverted)

        isOutlined =
            Just "is-outlined"
                |> Maybe.filter (\_ -> attrs |> List.member IsOutlined)
    in
    [ Just "button", isLoading, isInverted, isOutlined ]
        |> List.filterMap identity


buttonElement : List (ButtonAttribute msg) -> HtmlElement msg
buttonElement attrs =
    attrs
        |> List.filterMap
            (\attr ->
                case attr of
                    CustomElement el ->
                        Just el

                    _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.withDefault Html.button


otherButtonAttrs : List (ButtonAttribute msg) -> List (Html.Attribute msg)
otherButtonAttrs attrs =
    let
        color =
            attrs
                |> List.filterMap
                    (\attr ->
                        case attr of
                            Color c ->
                                Just c

                            _ ->
                                Nothing
                    )
                |> List.head
                |> Maybe.map buttonColor

        size =
            attrs
                |> List.filterMap
                    (\attr ->
                        case attr of
                            Size s ->
                                Just s

                            _ ->
                                Nothing
                    )
                |> List.head
                |> Maybe.map buttonSize

        italics =
            Just (style "font-style" "italic")
                |> Maybe.filter (\_ -> attrs |> List.member Italic)

        otherAttrs =
            attrs
                |> List.filterMap
                    (\attr ->
                        case attr of
                            CustomAttrs custom ->
                                Just custom

                            _ ->
                                Nothing
                    )
                |> List.concat
    in
    [ color, size, italics ]
        |> List.filterMap identity
        |> (++) otherAttrs


buttonColor : ButtonColor -> Html.Attribute msg
buttonColor color =
    case color of
        IsPrimary ->
            class "is-primary"

        IsDanger ->
            class "is-danger"

        IsInfo ->
            class "is-info"

        CustomColor c ->
            style "color" <| ColorExt.toCssString c


buttonSize : ButtonSize -> Html.Attribute msg
buttonSize size =
    class <|
        case size of
            Small ->
                "is-small"

            Normal ->
                "is-normal"

            Medium ->
                "is-medium"

            Large ->
                "is-large"


type GroupAlignment
    = AlignCenter
    | AlignLeft
    | AlignRight


type alias Group msg =
    { alignment : GroupAlignment
    , connected : Bool
    , buttons : List (Html msg)
    }


group : Group msg -> Html msg
group data =
    let
        groupClasses =
            [ Just "buttons"
            , groupAlignment data.alignment
            , Just "has-addons"
                |> Maybe.filter (\_ -> data.connected)
            ]
    in
    div
        [ class
            (groupClasses
                |> List.filterMap identity
                |> String.join " "
            )
        ]
        data.buttons


groupAlignment : GroupAlignment -> Maybe String
groupAlignment alignment =
    case alignment of
        AlignLeft ->
            Nothing

        AlignCenter ->
            Just "is-centered"

        AlignRight ->
            Just "is-right"
