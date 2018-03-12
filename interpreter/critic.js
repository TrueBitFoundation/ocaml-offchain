
var stack = []
var step = 0
var target = 10000

function pushCritical(loc) {
    if (step > target) return
    stack.push({step: step, loc:loc})
    if (target == step) console.log(stack)
    step++
}

function popCritical() {
    if (step > target) return
    if (target == step) console.log(stack)
    stack.pop()
    step++
}

function popLoopCritical(loc) {
    if (step > target || stack.length == 0) return
    var last = stack[stack.length-1]
    if (last.loc != loc) return
    if (Math.random() < 0.0001) console.log(stack, step)
    stack.pop()
    step++
}

function startBlock(loc) {
    if (step > target) return
    // console.log("Start " + loc)
    stack.push({step: step, loc:loc})
    if (target == step) console.log(stack)
    step++
    // console.log(stack)
}

function endBlock(loc) {
    if (step > target) return
    // console.log("End " + loc)
    if (stack.length == 0) {
        console.log("Trying to pop from empty stack!")
        return
    }
    if (target == step) console.log(stack)
    var last = stack[stack.length-1]
    stack.length--
    while (stack.length != 0 && last.loc != loc) {
        last = stack[stack.length-1]
        stack.length--
    }
    step++
}

Module.asmLibraryArg.pushCritical = pushCritical
Module.asmLibraryArg.popCritical = popCritical
Module.asmLibraryArg.popLoopCritical = popLoopCritical

Module.asmLibraryArg.startCritical = startBlock
Module.asmLibraryArg.endCritical = endBlock

