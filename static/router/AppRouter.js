// Router
(function ( router ) { 

  router.AppRouter = Backbone.Router.extend({

    routes:{
        "":"list",
        "feedings/new":"newFeeding",
        "feedings/:id":"feedingDetails"
    },

    initialize:function () {
        $('#header').html(app.views.header.render().el);
    },

    list:function () {
        this.feedingList = app.collections.paginatedItems;
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
        this.feedingView = new app.views.ItemView({model:this.feeding});
        $('#content').html(this.feedingView.render().el);
    },

    newFeeding:function () {
        if (app.feedingView) app.feedingView.close();
        app.feedingView = new app.views.ItemView({model:new Feeding()});
        $('#content').html(app.feedingView.render().el);
    }
  });
})( app.router );


