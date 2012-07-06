(function (views) {
  views.HeaderView = Backbone.View.extend({
 
    template:_.template($('#tpl-header').html()),
 
    initialize:function () {
        this.render();
    },
 
    render:function (eventName) {
        $(this.el).html(this.template());
        return this;
    },
 
    events:{
        "click .new":"newFeeding"
    },
 
    newFeeding: function(event) {
      app.navigate("feedings/new", true);
      return false;
    }
  });
})( app.views );
