(function (views) {

	views.PaginationView = Backbone.View.extend({

		events: {
			'click a.first': 'gotoFirst',
			'click a.prev': 'gotoPrev',
			'click a.next': 'gotoNext',
			'click a.last': 'gotoLast',
		},

		tagName: 'aside',

		pagingTemplate: _.template($('#tmpClientPagination').html()),

		initialize: function () {

			this.collection.on('reset', this.render, this);
			this.collection.on('change', this.render, this);
			this.$el.appendTo('#pagination');

		},
		render: function () {
			var html = this.pagingTemplate(this.collection.info());
			this.$el.html(html);
		},

		gotoFirst: function (e) {
			e.preventDefault();
			this.collection.goTo(1);
		},

		gotoPrev: function (e) {
			e.preventDefault();
			this.collection.previousPage();
		},

		gotoNext: function (e) {
			e.preventDefault();
			this.collection.nextPage();
		},

		gotoLast: function (e) {
			e.preventDefault();
			this.collection.goTo(this.collection.information.lastPage);
		},

	});
})( app.views );
