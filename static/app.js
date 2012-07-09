//Top-level namespaces for our code
(function(){

  //init
  window.app = {};
  app.collections = {};
  app.models = {};
  app.views = {};
  app.router = {};
  app.mixins = {};

// Defer initialization until doc ready.
  $(function(){
    app.collections.paginatedItems = new app.collections.PaginatedCollection();
    app.views.app = new app.views.AppView({collection: app.collections.paginatedItems});
    app.views.pagination = new app.views.PaginationView({collection: app.collections.paginatedItems});
    app.views.header = new app.views.HeaderView();
    window.app = new app.router.AppRouter({collection: app.collections.paginatedItems});
    Backbone.history.start(); //start the backbone router
  });

})();
