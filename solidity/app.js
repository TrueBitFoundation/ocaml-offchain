
var fs = require("fs")
// var exec = require('child_process').exec
var http = require('http').createServer()
var io = require("socket.io")(http)
var Web3 = require('web3')
var web3 = new Web3()
var execFile = require('child_process').execFile
var ipfsAPI = require('ipfs-api')

// connect to ipfs daemon API server
var ipfs = ipfsAPI('programming-progress.com', '5001', {protocol: 'http'})

web3.setProvider(new web3.providers.HttpProvider('http://localhost:8545'))

var base = "0x90f8bf6a479f320ead074411a4b0e7944ea8c9c1"

var code = fs.readFileSync("contracts/Tasks.bin")
var abi = JSON.parse(fs.readFileSync("contracts/Tasks.abi"))

var send_opt = {from:base, gas: 4000000}

var contract = new web3.eth.Contract(abi, "0xe78A0F7E598Cc8b0Bb87894B0F60dD2a88d6a8Ab")

io.on("connection", function(socket) {
    console.log("Got client")
    io.emit("client", {})
    socket.on("msg", function (str) {
        console.log(str)
    })
    socket.on("new_task", function (obj) {
        console.log(obj)
        var arr = {path:"task.wast", content:new Buffer(obj)}
        // store into IPFS, get ipfs address
        ipfs.files.add(new Buffer(obj), function (err, res) {
            if (err) {
                console.log(err)
                return
            }
            console.log(res)
            var filename = res[0].hash + ".wast"
            // store into filesystem
            fs.writeFile(filename, obj, function () {
                // run init script
                execFile('../interpreter/wasm', ["-m", "-init", "0", filename], (error, stdout, stderr) => {
                    if (error) {
                        console.error('stderr', stderr)
                        return
                    }
                    console.log('stdout', stdout)
                    // It should give the initial state hash, post it to contract
                    // contract.methods.add(JSON.parse(stdout), res.hash).send(send_opt)
                    contract.methods.add(JSON.parse(stdout), res[0].hash).send(send_opt)
                })
            })
        })
    })
})

// We should listen to contract events

http.listen(22448, function(){
    console.log("listening on *:22448")
})

