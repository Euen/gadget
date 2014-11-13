  Main = {
      onUrl : "/on",
      offUrl : "/off",
      login : function(e) {
          window.location.href = "/login";
      },
      toggle : function(e) {
          var button = $(this);
          var repo = button.attr('data-repo');
          var url = button.is('.on') ? Main.offUrl : Main.onUrl;
          var data = {'repo' : repo};
          $.get(url, data)
              .done(function() {Main.toggleStatus(button); })
              .fail(Main.error);
      },
      toggleStatus : function(button) {
          if(button.is('.on'))
              button.removeClass('on').addClass('off');
          else
              button.removeClass('off').addClass('on');
      },
      error : function() {
          alert("There was an error while trying to modify the repository.");
      }
  };

$(function() {
    $("#login").on('click', Main.login);

    $("button.toggle").on('click', Main.toggle);
});


function qs(key) {
    key = key.replace(/[*+?^$.\[\]{}()|\\\/]/g, "\\$&");
    var match = location.search.match(new RegExp("[?&]"+key+"=([^&]+)(&|$)"));
    return match && decodeURIComponent(match[1].replace(/\+/g, " "));
}
