  Main = {
      url : "/active-tools",
      login : function(e) {
          window.location.href = "/login";
      },
      toggle : function(e) {
          var button = $(this);
          var repo = button.attr('data-repo');
          var toolName = button.attr('data-tool');
          var data = JSON.stringify({'tool': toolName, 'repo': repo});
          var url = (button.is('.on') ? Main.url+"/"+toolName+"?repo="+repo : Main.url);
          var type = (button.is('.on') ? "DELETE" : "POST");

          $.ajax({
            type: type,
            url: url,
            contentType: "application/json",
            data: data
          })
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
