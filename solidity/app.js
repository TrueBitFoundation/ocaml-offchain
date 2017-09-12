
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

web3.setProvider(new web3.providers.HttpProvider('http://programming-progress.com:8545'))

// var base = "0x90f8bf6a479f320ead074411a4b0e7944ea8c9c1"
var base = "0x9acbcf2d9bd157999ae5541c446f8d6962da1d4d"

var code = fs.readFileSync("contracts/Tasks.bin")
var abi = JSON.parse(fs.readFileSync("contracts/Tasks.abi"))

var send_opt = {from:base, gas: 4000000}

// var contract = new web3.eth.Contract(abi, "0xe78A0F7E598Cc8b0Bb87894B0F60dD2a88d6a8Ab")
var contractABI = web3.eth.contract(abi)
var contract = contractABI.at("0x7ff09ea372d90b12325ff084526e48c16d52a1bb")

io.on("connection", function(socket) {
    console.log("Got client")
    io.emit("client", {})
    socket.on("msg", function (str) {
        console.log(str)
    })
    socket.on("new_task", function (obj) {
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
                    contract.add(JSON.parse(stdout), res[0].hash, send_opt, function (err, tr) {
                        if (err) console.log(err)
                        else {
                            console.log("Success", tr)
                            io.emit("task_success", tr)
                        }
                    })
                })
            })
        })
    })
})

function solveTask(obj) {
    var filename = obj.filehash + ".wast"
    // store into filesystem
    fs.writeFile(filename, obj.file, function () {
        execFile('../interpreter/wasm', ["-m", "-init", "0", filename], (error, stdout, stderr) => {
            if (error) {
                console.error('stderr', stderr)
                return
            }
            var inithash = JSON.parse(stdout)
            if (inithash == obj.hash) {
                console.log("Initial hash matches")
            }
            else {
                console.log("Initial hash was wrong")
                return
            }
            execFile('../interpreter/wasm', ["-m", "-result", "0", filename], function (error, stdout, stderr) {
                if (error) {
                    console.error('stderr', stderr)
                    return
                }
                var res = JSON.parse(stdout)
                contract.solve(obj.id, res.result, res.steps, send_opt, function (err, tr) {
                        if (err) console.log(err)
                        else {
                            console.log("Success", tr)
                            io.emit("solve_success", tr)
                        }
                    })
            })
        })
    })
}

function verifyTask(obj) {
    var filename = obj.filehash + ".wast"
    // store into filesystem
    fs.writeFile(filename, obj.file, function () {
        execFile('../interpreter/wasm', ["-m", "-init", "0", filename], (error, stdout, stderr) => {
            if (error) {
                console.error('stderr', stderr)
                return
            }
            var inithash = JSON.parse(stdout)
            if (inithash == obj.init) {
                console.log("Initial hash matches")
            }
            else {
                console.log("Initial hash was wrong")
                return
            }
            execFile('../interpreter/wasm', ["-m", "-result", "0", filename], function (error, stdout, stderr) {
                if (error) {
                    console.error('stderr', stderr)
                    return
                }
                var res = JSON.parse(stdout)
                if (res.result != obj.hash) console.log("Result mismatch")
                else if (res.steps != obj.steps) console.log("Wrong number of steps")
                else console.log("Seems correct")
            })
        })
    })
}

function getFile(fileid, cont) {
    ipfs.get(fileid, function (err, stream) {
        if (err) {
            console.log(err)
            return
        }
        var chunks = []
        stream.on('data', (file) => {
            file.content.on("data", function (chunk) {
                chunks.push(chunk);
            })
            file.content.on("end", function () {
                cont(Buffer.concat(chunks).toString())
            })
        })
    })
}

// var ev = contract.Posted({}, {fromBlock:0, toBlock:"latest"}, function () { console.log("here") }).get(function (err, res) { console.log(res) })
contract.Posted("latest").watch(function (err, ev) {
    if (err) console.log(err)
    io.emit("posted", {giver: ev.args.giver, hash: ev.args.hash, file:ev.args.file, id:ev.args.id.toString()})
    // download file from IPFS
    getFile(ev.args.file, function (filestr) {
        solveTask({giver: ev.args.giver, hash: ev.args.hash, file:filestr, filehash:ev.args.file, id:ev.args.id.toString()})
    })
})

contract.Solved("latest").watch(function (err, ev) {
    if (err) console.log(err)
    io.emit("solved", {hash: ev.args.hash, file:ev.args.file, init: ev.args.init, id:ev.args.id.toString(), steps:ev.args.steps.toString()})
    getFile(ev.args.file, function (filestr) {
        verifyTask({hash: ev.args.hash, file: filestr, filehash:ev.args.file, init: ev.args.init, id:ev.args.id.toString(), steps:ev.args.steps.toString()})
    })
})

// We should listen to contract events

http.listen(22448, function(){
    console.log("listening on *:22448")
})

