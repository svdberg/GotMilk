// Models
window.Feeding = Backbone.Model.extend({
  idAttribute: "_id",
  urlRoot:"../api/feedings",
  defaults:{
        "date": new Date(),
        "side":"",
        "time":"",
        "excrement":"P",
        "remarks":""
    }
});

window.FeedingCollection = Backbone.Collection.extend({
    model:Feeding,
    url:"../api/feedings"
});


// Views
window.FeedingListView = Backbone.View.extend({

    tagName:'table',

    initialize:function () {
        this.model.bind("reset", this.render, this);
        var self = this;
        this.model.bind("add", function (feeding) {
            $(self.el).append(new FeedingListItemView({model:feeding}).render().el);
        });
    },
 
    render:function (eventName) {
        _.each(this.model.models, function (feeding) {
            $(this.el).append(new FeedingListItemView({model:feeding}).render().el);
        }, this);
        return this;
    }
});

window.FeedingListItemView = Backbone.View.extend({

    tagName:"tr",

    template:_.template($('#tpl-feeding-list-item').html()),

    initialize:function () {
        this.model.bind("change", this.render, this);
        this.model.bind("destroy", this.close, this);
    },

    render:function (eventName) {
        $(this.el).html(this.template(this.model.toJSON()));
        return this;
    },

    close:function () {
        $(this.el).unbind();
        $(this.el).remove();
    }

});

window.FeedingView = Backbone.View.extend({

    template:_.template($('#tpl-feeding-details').html()),

    initialize:function () {
        this.model.bind("change", this.render, this);
    },
 
    render:function (eventName) {
        $(this.el).html(this.template(this.model.toJSON()));
        return this;
    },
 
    events:{
        "change input":"change",
        "click .save":"saveFeeding",
        "click .delete":"deleteFeeding"
    },
 
    change:function (event) {
        var target = event.target;
        console.log('changing ' + target.id + ' from: ' + target.defaultValue + ' to: ' + target.value);
        // You could change your model on the spot, like this:
        // var change = {};
        // change[target.name] = target.value;
        // this.model.set(change);
    },
 
    saveFeeding:function () {
        this.model.set({
            date:$('#date').val(),
            side:$('#side').val(),
            time:$('#time').val(),
            excrement:$('#excrement').val(),
            remarks:$('#remarks').val()
        });
        if (this.model.isNew()) {
          console.log('new model with id: ' + this.model.id);
          var self = this;
          app.feedingList.create(this.model, {
            success: function() {
              app.navigate('feedings/'+self.model.id, false);
            }
          });
        } else {
          console.log('existing model with id: ' + this.model.id);
          this.model.save();
        }
        return false;
    },
 
    deleteFeeding:function () {
        this.model.destroy({
            success:function () {
                alert('Feeding deleted successfully');
                window.history.back();
            }
        });
        return false;
    },
 
    close:function () {
        $(this.el).unbind();
        $(this.el).empty();
    }
});

window.HeaderView = Backbone.View.extend({
 
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

// Router
var AppRouter = Backbone.Router.extend({

    routes:{
        "":"list",
        "feedings/new":"newFeeding",
        "feedings/:id":"feedingDetails"
    },

    initialize:function () {
        $('#header').html(new HeaderView().render().el);
    },

    list:function () {
        this.feedingList = new FeedingCollection();
        var self = this;
        this.feedingList.fetch({
            success:function () {
                self.feedingListView = new FeedingListView({model:self.feedingList});
                $('#sidebar').html(self.feedingListView.render().el);
                if (self.requestedId) self.feedingDetails(self.requestedId);
            }
        });
    },

    feedingDetails:function (id) {
        this.feeding = this.feedingList.get(id);
        if (app.feedingView) app.feedingView.close();
        this.feedingView = new FeedingView({model:this.feeding});
        $('#content').html(this.feedingView.render().el);
    },

    newFeeding:function () {
        if (app.feedingView) app.feedingView.close();
        app.feedingView = new FeedingView({model:new Feeding()});
        $('#content').html(app.feedingView.render().el);
    }
});

var app = new AppRouter();
Backbone.history.start();