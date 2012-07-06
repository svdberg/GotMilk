(function (collections, model, paginator) {

	collections.PaginatedCollection = paginator.clientPager.extend({

		model: model,

		paginator_core: {
			// the type of the request (GET by default)
			type: 'GET',
			
			// the type of reply (jsonp by default)
			dataType: 'json',
		
			// the URL (or base URL) for the service
			url: '../api/feedings'
		},
		
		paginator_ui: {
			// the lowest page index your API allows to be accessed
			firstPage: 1,
		
			// which page should the paginator start from 
			// (also, the actual page the paginator is on)
			currentPage: 1,
			
			// how many items per page should be shown
			perPage: 10,
			
			// a default number of total pages to query in case the API or 
			// service you are using does not support providing the total 
			// number of pages for us.
			// 10 as a default in case your service doesn't return the total
			totalPages: 10
		}
		
	});

})( app.collections, app.models.Feeding, Backbone.Paginator);
