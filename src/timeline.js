import * as d3 from './d3.v5.min'

export function setupTimeline(elementId) {
  //need the should attends to show the faded dots, which prob actually won't be faded
  var timeline = d3.select(`#${elementId}`)
  console.log(timeline)

  var height = 500
  var circleX = 100
  var circleR = 9
  var circleLineWidth = 2
  var timelineLineWidth = 5

  var parseTime = d3.timeParse('%Q')
  var y = d3.scaleTime().range([height - 20, 10])

  d3.json('https://gleeclub.gatech.edu/cgi-bin/api/week_of_events').then(
    function(data) {
      console.log(data)
      //this needs to go from monday to sunday
      var now = new Date()
      var monday = d3.timeMonday()
      var sunday = new Date()
      sunday.setTime(monday.getTime() + 7 * 86400000 - 1)
      y.domain([sunday, monday])

      data.forEach(function(d, i) {
        d.callTime = parseTime(d.callTime)
        d.yPos = y(d.callTime)
      })

      timeline
        .append('g')
        .attr('transform', 'translate(' + (circleX - 1) + ',0)')
        // .call(d3.axisLeft(y).ticks(0).tickSizeOuter(0));
        .call(
          d3
            .axisLeft(y)
            .ticks(7)
            .tickFormat(d3.timeFormat('%a'))
            .tickSizeOuter(0),
        )

      //do something neat when an event is now
      timeline
        .selectAll('circle')
        .data(data)
        .enter()
        .append('circle')
        .attr('class', 'dot')
        .attr('cy', function(d, i) {
          if (i == 0) return d.yPos
          if (i >= 1 && y(d.callTime) - y(data[i - 1].callTime) > 20) {
            return y(d.callTime)
          } else return circleR * -1
        })
        .attr('cx', circleX)
        .attr('r', circleR)
        .attr('stroke-width', circleLineWidth)
      timeline
        .append('circle')
        .attr('class', 'dot now')
        .attr('cy', function(d) {
          return y(now)
        })
        .attr('cx', circleX - 0.5)
        .attr('r', timelineLineWidth / 2)

      timeline
        .selectAll('p')
        .data(data)
        .enter()
        .append('a')
        .attr('href', function(d) {
          return '#/events/' + d.id
        })
        .append('text')
        .text(function(d) {
          return d.name
        })
        .attr('y', function(d, i) {
          if (i == 0) return d.yPos + circleR / 2.0
          if (i > 0 && d.yPos - data[i - 1].yPos > 20) {
            return d.yPos + circleR / 2.0
          } else {
            d.yPos = data[i - 1].yPos + 16
            return d.yPos + circleR / 2.0 //what's the font size? add half the font size? 16px
          }
        })
        .attr('x', circleX + 15)
    },
  )
}
