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
    $.each(arr, function(idx, val) {
      result.push([Date.parse(val.createdAt), val.count]);
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
        type: 'column'
      },
      plotOptions: {
        column: {
          stacking: 'normal'
        }
      },
      xAxis: {
        type: 'datetime',
        dateTimeLabelFormats: {
          month: '%Y-%m-%d',
          year: '%Y'
        }
      },
      yAxis: {
        stackLabels: {
          enabled: true
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
