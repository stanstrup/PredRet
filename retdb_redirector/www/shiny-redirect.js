setInterval(function() {
  var link = document.getElementById('link').value;
    if (link.length >1) {
      window.open(link, "_top")
    }
}, 50)