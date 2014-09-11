  $(function() {
    var time_now = new Date()
    $("input#client_time").val(time_now.getTime())
    $("input#client_time_zone_offset").val(time_now.getTimezoneOffset())
  });
