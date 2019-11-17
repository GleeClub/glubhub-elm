module Graph exposing (EventHovered, graphGrades)

-- import Color

import Axis
import Color exposing (Color)
import Html exposing (text)
import Models.Event exposing (FullEvent)
import Models.Info exposing (Semester)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Time exposing (Posix)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (class, cx, cy, fill, r, stroke, title, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events exposing (onMouseEnter, onMouseLeave)
import TypedSvg.Types exposing (Fill(..), Length(..), Transform(..))


w : Float
w =
    900


h : Float
h =
    500


goldColor : Color
goldColor =
    Color.rgb 0.706 0.643 0.412


padding : Float
padding =
    30


xScale : Semester -> ContinuousScale Time.Posix
xScale semester =
    Scale.time Time.utc ( 0, w - 2 * padding ) ( semester.startDate, semester.endDate )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 100 )


xAxis : Semester -> List ( Posix, Float ) -> Svg msg
xAxis semester events =
    Axis.bottom [ Axis.tickCount (List.length events // 2) ] (xScale semester)


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


transformToLineData : Semester -> ( Posix, Float ) -> Maybe ( Float, Float )
transformToLineData semester ( callTime, partialScore ) =
    Just ( Scale.convert (xScale semester) callTime, Scale.convert yScale partialScore )


transformToAreaData : Semester -> ( Posix, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
transformToAreaData semester ( callTime, partialScore ) =
    Just
        ( ( Scale.convert (xScale semester) callTime, Tuple.first (Scale.rangeExtent yScale) )
        , ( Scale.convert (xScale semester) callTime, Scale.convert yScale partialScore )
        )


eventPoint : Semester -> (Maybe EventHovered -> msg) -> FullEvent -> Svg msg
eventPoint semester hoverMsg event =
    let
        xPos =
            Scale.convert (xScale semester) event.callTime

        yPos =
            Scale.convert yScale (eventPartialScore event)
    in
    circle
        [ r <| Px 3
        , fill (Fill goldColor)
        , onMouseEnter (hoverMsg <| Just { event = event, x = round xPos, y = round yPos })
        , onMouseLeave (hoverMsg Nothing)
        , cx <| Px xPos
        , cy <| Px yPos
        ]
        []


line : Semester -> List ( Posix, Float ) -> Path
line semester pastEvents =
    pastEvents
        |> List.map (transformToLineData semester)
        |> Shape.line Shape.monotoneInXCurve


area : Semester -> List ( Posix, Float ) -> Path
area semester pastEvents =
    pastEvents
        |> List.map (transformToAreaData semester)
        |> Shape.area Shape.monotoneInXCurve


eventPartialScore : FullEvent -> Float
eventPartialScore event =
    event.attendance |> Maybe.map .partialScore |> Maybe.withDefault 0


graphGrades : Semester -> List FullEvent -> (Maybe EventHovered -> msg) -> Svg msg
graphGrades semester events hoverMsg =
    let
        pastEvents =
            case ( events |> List.head, events |> List.reverse |> List.head ) of
                ( Just firstEvent, Just lastEvent ) ->
                    [ ( firstEvent.callTime, 0 ) ] ++ (events |> List.map (\event -> ( event.callTime, eventPartialScore event ))) ++ [ ( lastEvent.callTime, 0 ) ]

                ( _, _ ) ->
                    []
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis semester pastEvents ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            ([ Path.element (area semester pastEvents) [ strokeWidth 2, fill <| Fill <| Color.rgba 1 0 0 0.54 ]
             , Path.element (line semester pastEvents) [ stroke goldColor, strokeWidth 2, fill FillNone ]
             ]
                ++ (events |> List.map (eventPoint semester hoverMsg))
            )
        ]


type alias EventHovered =
    { event : FullEvent
    , x : Int
    , y : Int
    }



-- <style>
-- .line {
--   stroke: #b4a46a;
--   fill: url("#attendanceGradient");
--   stroke-width: 2;
-- }
-- .attendanceDot {
--   fill: #b4a46a;
-- }
-- </style>
