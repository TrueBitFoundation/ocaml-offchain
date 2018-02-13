const jayson = require('jayson');

// create a server
const server = jayson.server({
  add: function(args, callback) {
    callback(null, args[0] + args[1]);
  }
});

console.log("Starting WASM Interpreter Server")
server.http().listen(3000);