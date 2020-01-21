module Graph exposing (HoveredEvent, graphGrades)

-- import Color

import Axis
import Color exposing (Color)
import Html.Events exposing (on)
import Json.Decode as Decode
import Models.Event exposing (Event)
import Models.Info exposing (Semester)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Time exposing (Posix)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (class, cx, cy, fill, r, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events exposing (onMouseLeave)
import TypedSvg.Types exposing (Fill(..), Length(..), Transform(..))
import Utils exposing (goldColor)


w : Float
w =
    900


h : Float
h =
    400


padding : Float
padding =
    30


gray : Color
gray =
    Color.rgb 0.7 0.7 0.7


xScale : Semester -> ContinuousScale Time.Posix
xScale semester =
    Scale.time Time.utc ( 0, w - 2 * padding ) ( semester.startDate, semester.endDate )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 100 )


xAxis : Semester -> Svg msg
xAxis semester =
    Axis.bottom [ Axis.tickCount 20 ] (xScale semester)


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


transformToLineData : Semester -> ( Posix, Float ) -> Maybe ( Float, Float )
transformToLineData semester ( callTime, partialScore ) =
    Just ( Scale.convert (xScale semester) callTime, Scale.convert yScale partialScore )


transformToAreaData : Semester -> ( Posix, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
transformToAreaData semester ( callTime, partialScore ) =
    let
        callTimeCoord =
            Scale.convert (xScale semester) callTime

        scoreLowerBound =
            Tuple.first (Scale.rangeExtent yScale)

        score =
            Scale.convert yScale partialScore
    in
    Just ( ( callTimeCoord, scoreLowerBound ), ( callTimeCoord, score ) )


eventPoint : Semester -> (Maybe HoveredEvent -> msg) -> Event -> Svg msg
eventPoint semester hoverMsg event =
    let
        xPos =
            Scale.convert (xScale semester) event.callTime

        yPos =
            Scale.convert yScale (eventPartialScore event)
    in
    circle
        [ r <| Px 2.5
        , stroke Color.black
        , strokeWidth 0.5
        , fill <| Fill goldColor
        , on "mousedown" (hoveredEventDecoder event |> Decode.map hoverMsg)
        , on "mouseenter" (hoveredEventDecoder event |> Decode.map hoverMsg)
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


eventPartialScore : Event -> Float
eventPartialScore event =
    event.gradeChange |> Maybe.map .partialScore |> Maybe.withDefault 0


graphGrades : Semester -> List Event -> (Maybe HoveredEvent -> msg) -> Svg msg
graphGrades semester events hoverMsg =
    let
        callTimesAndScores =
            events |> List.map (\event -> ( event.callTime, eventPartialScore event ))

        pastEvents =
            case ( callTimesAndScores |> List.head, callTimesAndScores |> List.reverse |> List.head ) of
                ( Just ( _, firstScore ), Just ( _, lastScore ) ) ->
                    ( semester.startDate, firstScore )
                        :: callTimesAndScores
                        ++ [ ( semester.endDate, lastScore ) ]

                ( _, _ ) ->
                    []
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis semester ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            Path.element (area semester pastEvents)
                [ strokeWidth 2, fill <| Fill gray ]
                :: Path.element (line semester pastEvents)
                    [ stroke goldColor, strokeWidth 2, fill FillNone ]
                :: (events |> List.map (eventPoint semester hoverMsg))
        ]



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


type alias HoveredEvent =
    { x : Int
    , y : Int
    , event : Event
    }


hoveredEventDecoder : Event -> Decode.Decoder (Maybe HoveredEvent)
hoveredEventDecoder event =
    Decode.map2 (\x y -> Just { x = x, y = y, event = event })
        (Decode.at [ "pageX" ] Decode.int)
        (Decode.at [ "pageY" ] Decode.int)
