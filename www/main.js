Shiny.addCustomMessageHandler('details', function(data) {
    switch(data) {
      case 'show' :
        $('#plot, #plotDetails').addClass('details')
        break
      case 'hide' :
        $('#plot, #plotDetails').removeClass('details')
        break
    }
    $(window).trigger('resize')
});

Shiny.addCustomMessageHandler('rglplotError', function(data) {
    console.log("addCustomMessageHandler")

    var $e = $('#rglplotError')
    $e.children().remove()

    if(!data) return;
  
    if(typeof data == "string") {
        $e.append(
          $(document.createElement('DIV')).text(data)
        )
    }
    else {
        for(var i=0; i<data.length; i++) {
          $e.append(
            $(document.createElement('DIV')).text(data[i])
          )
        }
    }
});
