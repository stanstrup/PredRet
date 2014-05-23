<script type="text/javascript">
  jQuery(document).ready(function () {
    jQuery("#IDofControlFiringResizeEvent").click(function () {
      var frame = $('#IDofiframeInMainWindow', window.parent.document);
      var height = jQuery("#IDofContainerInsideiFrame").height();
      frame.height(height + 15);
    });
  });
</script>