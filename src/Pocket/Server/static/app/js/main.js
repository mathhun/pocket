;(function ($) {
  'use strict';

  var testdata = {
    list: [
      { createdAt: '2014-08-07 22:00', status:'added', count: 1},
      { createdAt: '2014-08-07 23:00', status:'added', count: 1},
      { createdAt: '2014-08-07 22:00', status:'favorite', count: 54},
      { createdAt: '2014-08-07 22:00', status:'keep', count: 3},
      { createdAt: '2014-08-07 22:00', status:'unread', count: 401}
    ]
  };

  var collectData = function(arr) {
    var result = [];
    var h = 2*9*60*60*1000;
    $.each(arr, function(idx, val) {
      result.push([Date.parse(val.createdAt)+h, val.count]);
    });
    return result;
  };

  var createChart = function(data) {
    var hash = {};
    for (var i = 0; i < data.length; i++) {
      var status = data[i].status;
      if (!hash[status]) hash[status] = [];

      hash[status].push(data[i]);
    };

    var series = [];
    $.each(hash, function(key, arr) {
      series.push({
        name: key,
        data: collectData(arr)
      });
    });

    console.log(series);

    return {
      chart: {
        animation: false,
        type: 'column'
      },
      title: { text: null },
      plotOptions: {
        column: {
          animation: false,
          stacking: 'normal'
        }
      },
      credits: { enabled: false },
      xAxis: {
        type: 'datetime',
        dateTimeLabelFormats: {
          day: '%m/%d',
          month: '%Y/%m/%d',
          year: '%Y'
        }
      },
      yAxis: {
        title: { text: null },
        stackLabels: { enabled: false }
      },
      tooltip: {
        formatter: function() {
          var label = Highcharts.dateFormat('%Y/%m/%d %H:00', this.x);
          return '<b>'+ label +'</b><br/>'+
            this.series.name +': '+ this.y +'<br/>'+
            'Total: '+ this.point.stackTotal;
        }
      },
      series: series,
    };
  };

  var apiUrl = 'http://localhost:3000/pocket';

  $.getJSON(apiUrl, function(data) {
    console.log(data);
    $('#chart').highcharts(createChart(data.list));
  });

})(jQuery);
