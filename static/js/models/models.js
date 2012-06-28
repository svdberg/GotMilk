window.Employee = Backbone.Model.extend();

window.EmployeeCollection = Backbone.Collection.extend({
  model:Employee
  url:"../api/employees"
});

