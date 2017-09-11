
var fs = require("fs")
var Web3 = require('web3')
var web3 = new Web3()

web3.setProvider(new web3.providers.HttpProvider('http://programming-progress.com:8545'))

var base = "0x9acbcf2d9bd157999ae5541c446f8d6962da1d4d"

var code = fs.readFileSync("contracts/Tasks.bin")
var abi = JSON.parse(fs.readFileSync("contracts/Tasks.abi"))

var send_opt = {from:base, gas: 4000000}

var sol_testContract = new web3.eth.Contract(abi)

sol_testContract.deploy({data:"0x"+code}).send(send_opt).then(function (contract) {
  console.log('Contract mined! address: ' + contract.options.address)
})

