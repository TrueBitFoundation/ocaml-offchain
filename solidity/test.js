
var Web3 = require('web3')
var web3 = new Web3()
var fs = require("fs")

web3.setProvider(new web3.providers.HttpProvider('http://localhost:8545'))

var base = "0x76fab5f3597241c73c2dd4daac85af7523395f87"

web3.eth.getBalance(base).then(balance => console.log(balance));

var code = fs.readFileSync("contracts/Instruction.bin")
var abi = JSON.parse(fs.readFileSync("contracts/Instruction.abi"))
var test = JSON.parse(fs.readFileSync("test.json"))

// console.log(test.states)

var send_opt = {from:base, gas: 4000000}

function checkFetch(c) {
    var vm = test.fetch.vm
    c.methods.select(0).send(send_opt).on("error", console.error)
    c.methods.setVM(vm.code, vm.stack, vm.memory, vm.break_stack1, vm.break_stack2, vm.globals, vm.call_stack, vm.calltable,
            vm.pc, vm.stack_ptr, vm.break_ptr, vm.call_ptr, vm.memsize).send(send_opt).on("error", console.error)
    c.methods.hashVM().call().then(b => console.log(b.toString(16)))
    // c.methods.proveFetch(test.fetch.location).call().then(b => console.log(b))
}

var sol_testContract = new web3.eth.Contract(abi)
sol_testContract.deploy({data:code, arguments:[test.states, base, base]}).send(send_opt).then(contract => {
   console.log('Contract mined! address: ' + contract.options.address)
   checkFetch(contract)
})

/*
var sol_test = sol_testContract.new(
   , function (e, contract){
    console.log(e, contract);
    if (typeof contract.address !== 'undefined') {
    }
 })
*/
    