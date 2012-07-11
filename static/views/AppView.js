( function ( views ) {

	views.AppView = Backbone.View.extend({

		el : '#content',

		initialize : function () {

			var feedings = this.collection;

			feedings.on('add', this.addOne, this);
			feedings.on('reset', this.addAll, this);
			feedings.on('all', this.render, this);
			
			feedings.fetch({
				success: function(){
					feedings.pager();
				},
				silent:true
			});


		},
		addAll : function () {
			this.$el.empty();
			this.collection.each (this.addOne);
		},
		
		addOne : function ( item ) {
			var view = new views.ResultView({model:item});
			$('#content').append(view.render().el);
		},

		render: function(){
		}
	});

})( app.views );

