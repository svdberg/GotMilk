(function ( models ) {
  models.Feeding = Backbone.Model.extend({
    idAttribute: "_id",
    defaults:{
          "date": new Date(),
          "side":"",
          "time":"",
          "excrement":"P",
          "remarks":""
    }
  });

})( app.models );
