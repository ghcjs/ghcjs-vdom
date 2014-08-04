var DataSet = require('data-set');


function EventHook(value) {
  if (!(this instanceof EventHook)) {
    return new EventHook(value);
  }
  this.value = value;
}

EventHook.prototype.hook = function (node, propertyName) {
  var ds = DataSet(node);
  ds[propertyName] = this.value;
};

module.exports = EventHook;
