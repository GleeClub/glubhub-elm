module Page.Home exposing (..)


import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, img, input, label, section, span, text)
import Html.Attributes exposing (class, href, id, placeholder, src, style, type_, value, attribute)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (find)
import Maybe.Extra exposing (filter, isJust)
import Models.Event exposing (FullEvent, fullEventDecoder)
import Route exposing (Route)
import Utils exposing (Common, RemoteData(..), getRequest, notFoundView, permittedTo, setToken, spinner)
import List.Extra exposing (last)


---- MODEL ----


type alias Model =
    { common : Common
    , events : RemoteData (List EventFull)
    , hoveredEvent : Maybe EventHovered
    }

type alias EventHovered =
    { eventId : Int
    , x : Int
    , y : Int
    }

init : Common -> ( Model, Cmd Msg )
init common =
    ( { common = common, events = Loading, hoveredEvent = Nothing }, loadEvents common )



---- UPDATE ----


type Msg
    = OnLoadEvents (Result Http.Error (List FullEvent))
    | HoverOverEvent Int
    | ClearHover


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoadEvents (Ok events) ->
            ( { model | events = Loaded events }, Cmd.none )
        
        OnLoadEvents (Err _) ->
            ( { model | events = Failure }, Cmd.none )

        HoverOverEvent eventId ->
            ( { model | selected = Just eventId }, Cmd.none )

        ClearHover ->
            ( { model | selected = Nothing }, Cmd.none )


---- DATA ----


loadEvents : Common -> Cmd Msg
loadEvents common =
    getRequest common "/events?full=true" (Http.expectJson OnLoadEvents (Decode.list <| fullEventDecoder))


pastAndFutureEvents : Common -> List FullEvent -> (List FullEvent, List FullEvent)
pastAndFutureEvents common allEvents =
    let
        allGrades =
            common.user.grades
                |> Maybe.map \grades -> grades.changes
                |> Maybe.withDefault []

        mostRecentPastEvent =
            allGrades
                |> List.last
                |> Maybe.map \change -> change.event

        mostRecentCallTime = 
            mostRecentPastEvent |> Maybe.map \event -> event.callTime
            
    in
    case mostRecentCallTime of
        Just callTime ->
            ( allEvents |> filter \e -> e.callTime <= callTime
            , allEvents |> filter \e -> e.callTime > callTime
            )
    
        _ ->
            ( allEvents, [] )


attendanceMessage : Maybe Grades -> String
attendanceMessage grades =
    case grades of
        Nothing ->
            "Do you even go here?"

        Just grades ->
            if grades.finalGrade >= 90.0 then
                "Ayy lamo nice."
            else if grades.finalGrade >= 80.0 then
                "Ok not bad, I guess."
            else if grades.finalGrade >= 70.0 then
                "Pls"
            else
                "BRUH get it together."


---- VIEW ----


view : Model -> Html Msg
view model =
    case model.events of
        NotAsked ->
            text ""
    
        Loading ->
            spinner

        Failure ->
            text "Shucks!"

        Loaded events ->
            let
                ( pastEvents, futureEvents ) =
                    pastAndFutureEvents model.common events

                gigRequirement =
                    model.common.user.grades
                        |> Maybe.map \g -> g.gigRequirement
                        |> Maybe.withDefault 5
            in
            div [ id "home" ] 
                [ gradesBlock pastEvents
                , section [ class "section" ]
                    [ div [ class "container" ]
                        [ div [ class "columns" ]
                            [ upcomingEvents futureEvents
                            , volunteerism pastEvents gigRequirement
                            ]
                        ]
                    ]   
                ]


gradesBlock : List FullEvent -> Html Msg
gradesBlock pastEvents =
    let
        finalGrade =
            pastEvents
                |> List.Extra.last
                |> Maybe.andThen \event -> event.attendance
                |> Maybe.map \attendance -> attendance.partialScore
                |> Maybe.withDefault 100.0

        roundedFinalGrade =
            (toFloat (grades.finalGrade * 100.0)) / 100.0

        attendanceIssueEmail =
            "mailto:gleeclub_officers@lists.gatech.edu?subject=Attendance%20Issue"

        scoreText = 
            p []
                [ text "Right now you have a"
                , strong [] [ text <| String.fromFloat roundedFinalGrade ]
                , text "."
                , br [] []
                , span [ class "has-text-grey-light is-italic" ]
                    [ text <| attendanceMessage grades ]
                ]

        graph =
            if List.length pastEvents == 0 then
                [ p [] [ text "New semester, new you! Make it count." ]
                , br [] []
                , br [] []
                ]
            else 
                [ svg [] [] -- TODO: the graph!
                , p [ id "attendance-issue" ]
                    [ br [] []
                    , text "Do you have an issue? Do you need a daddy tissue?"
                    , a [ href attendanceIssueEmail ]
                        [ text "Email the officers" ]
                    , text "to cry about it."
                    ]
                ]
    in

    section [ class "section" ]


        [ div [ class "container", id "grades-container" ]
            [ h1 [ class "title" ] [ text "Score" ]
            , p []
                [ text "Right now you have a"
                , strong [] [ text <| String.fromFloat finalGrade ]
                , text "."
                , br [] []
                , span [ class "has-text-grey-light is-italic" ]
                    [ text <| attendanceMessage grades ]
                ]
            , svg [] [] -- TODO: the graph!
            , p [ id "attendance-issue" ]
                [ br [] []
                , text "Do you have an issue? Do you need a daddy tissue?"
                , a [ href attendanceIssueEmail ]
                    [ text "Email the officers" ]
                , text "to cry about it."
                ]
            ]
        ]


upcomingEvents : List FullEvent -> Html Msg
upcomingEvents futureEvents =
    let
        eventList =
            if List.length futureEvents == 0 then
                [ p [] [ text "No more events this semester (:(" ] ]
            else
                futureEvents
                    |> List.take 5
                    |> List.indexedMap \index, event -> (
                        p [] 
                            [ span [ class "tag is-primary is-rounded" ]
                                [ text <| String.fromInt (index + 1) ]
                            , a [ Route.href Event { id = Just event.id, tab = Nothing } ]
                                [ text event.name
                                , text "–"
                                , text <| formatCallTime event.callTime -- moment's fromNow was used
                                ]
                            ]
                    )
    in
    div [ class "column" ]
        [ h1 [ class "title" ] [ text "Next Up" ]
        , div [ class "box" ] eventList
        ]


volunteerism : List FullEvent -> Int -> Html Msg
volunteerism pastEvents gigRequirement =
    let
        volunteerGigsAttended =
            pastEvents
                |> List.filter \event -> event.gigCount and event.attendance.didAttend

        partialGigList =
            volunteerGigsAttended
                |> List.map \gig -> Just gig
                |> ++ List.repeat gigRequirement Nothing
                |> List.take gigRequirement

        tooltipText maybeGig = 
            maybeGig 
                |> Maybe.map \gig -> gig.name ++ " on " ++ formatCallTimeDateOnly gig
                |> Maybe.withDefault "Hopefully something soon..."

        gigIcon maybeGig =
            i [ class <| "fas fa-2x " ++ if isJust maybeGig then "fa-check-circle" else "fa-frown" ] []
    in
    div [ class "column" ]
        [ h1 [ class "title" ] [ text "Volunteerism" ]
        , div [ class "box" ]
            [ p []
                [ text "OK so you've only been to"
                , text <| romanNumeral volunteerGigsAttended
                , text "volunteer gigs this semester and you need to go to"
                , text <| romanNumeral gigRequirement
                , text ". So. Uh, you know, do that."
                ]
            , p [ style "text-align" "center" ] 
                ( partialGigList |> List.map \gig ->
                    span
                        [ class "icon is-large tooltip is-tooltip has-text-primary"
                        , attribute "data-tooltip" <| tooltipText gig
                        ]
                        [ gigIcon gig ]
                )
            ]
        ]




public drawAttendanceGraph(): void {
    // var self = this;
    const margin = { top: 20, right: 20, bottom: 20, left: 24 };
    const width = (d3.select('grades-container').node() as Element).clientWidth;
    const height = width * 0.7;
    const svg = d3.select('svg')
      .attr('width', width)
      .attr('height', height)
      .attr('transform', 'translate(' + margin.left / 2 + ', ' + margin.top / 2 + ')');
    const gradient = d3.select('svg').append('defs')
      .append('linearGradient')
      .attr('id', 'attendanceGradient')
      .attr('gradientTransform', 'rotate(90)');
    gradient.append('stop')
      .attr('offset', '0%')
      .attr('stop-color', 'lightgrey');
    gradient.append('stop')
      .attr('offset', '100%')
      .attr('stop-color', 'darkgrey');
    const div = d3.select('#tooltip')
      .attr('class', 'box')
      .attr('class', 'hidden');

    const pastEvents = this.past
      .concat(this.future)
      .filter((event) => event.gradeChange !== null);
    const xScale = d3.scaleTime()
      .rangeRound([margin.left, width - margin.right])
      .domain(d3.extent(pastEvents, (d) => d.callTime));
    const yScale = d3.scaleLinear()
      .rangeRound([height - margin.bottom, margin.top])
      .domain([0, 100]);

    svg.append('g')
      .attr('transform', `translate(0,${height - margin.bottom})`)
      .call(d3.axisBottom(xScale).ticks(3));
    svg.append('g')
      .attr('transform', `translate(${margin.left}, 0)`)
      .call(d3.axisLeft(yScale));

    const eventsForLine = pastEvents.map((event) => [event.callTime, event.partialScore]);
    const valueLine = d3.line()
        .x(([callTime, partialScore]) => xScale(callTime))
        .y(([callTime, partialScore]) => yScale(partialScore > 0 ? partialScore : 0))
        .curve(d3.curveMonotoneX); // http://bl.ocks.org/d3indepth/b6d4845973089bc1012dec1674d3aff8
    eventsForLine.unshift([pastEvents[0].callTime, 0]);
    eventsForLine.push([pastEvents[pastEvents.length - 1].callTime, 0]);

    svg.append('path')
      .datum(eventsForLine)
      .attr('class', 'line')
      .attr('d', valueLine);

    const circleSelect = svg.selectAll('circle')
      .data(pastEvents)
      .enter();
    circleSelect
      .append('circle')
      .attr('cx', (d) => xScale(d.callTime))
      .attr('cy', (d) => yScale(d.partialScore > 0 ? d.partialScore : 0))
      .attr('r', () => 4)
      .attr('class', 'attendanceDot')
      .attr('stroke-width', 3);
    circleSelect
      .append('circle')
      .attr('cx', (d) => xScale(d.callTime))
      .attr('cy', (d) => yScale(d.partialScore > 0 ? d.partialScore : 0))
      .attr('r', () => 8)
      .attr('fill-opacity', '0')
      .on('mouseover touchdown', (d) => {
        div.attr('class', 'box shown');
        div.append('p').html('<strong>' + d.name + '</strong>');
        div.append('p').html(moment(d.callTime).format(this.dateFmtLong));
        div.append('p').html(
          `${Number(d.gradeChange.toFixed(2))} points
           <span v-else class='icon is-primary has-text-primary'>
            <i class='fas fa-arrow-right'></i>
           </span> ${Number(d.partialScore.toFixed(2))}%`);
        div.append('p').html('<em>' + d.gradeChangeReason + '</em>');
        div.attr('style', `position:absolute;left:${d3.event.pageX}px;top:${d3.event.pageY}px;`);
      })
      .on('mouseout touchup', () => {
        div.attr('class', 'hidden');
        div.html('');
      });
  }


  public mounted(): void {
    this.common.apiGet<EventFull[]>('events', { full: true }, (events) => {
      const now = moment().unix();
      this.past = events.filter((e) => e.callTime < now);
      this.future = events.filter((e) => e.callTime >= now);
      if (this.past.length) {
        this.drawAttendanceGraph();
      } else {
        const newp = d3.select('.container').insert('p', 'svg');
        newp.html('New semester, new you! Make it count.');
        d3.select('svg').remove();
        d3.select('#tooltip').remove();
        d3.select('.container').select('p').append('br');
        d3.select('.container').select('p').append('br');
        d3.select('#attendanceIssue').remove();
      }
    });
  }
}
</script>

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

