
// Building the stack

// we should only need the critical path
var path = JSON.parse(fs.readFileSync("critical.out"))

var step = 0

var fstack = []

function startCritical(loc) {
    // check if we are on the critical path
    if (path.length == 0) return false
    step++
    var el = path[0]
    if (el.loc == loc) {
        console.log("found path", el.loc)
        path = path.slice(1)
        return true
    }
    return false
}

var stack = []

Module.asmLibraryArg.adjustStack0 = function (num) {
    console.log("adjust", num)
    // stack.length -= num
}

Module.asmLibraryArg.adjustStackI32 = function (v, num) {
    // stack.length -= num
    stack.push(v)
    console.log("adjust i32", v, num)
    return v
}

Module.asmLibraryArg.adjustStackF32 = function (v, num) {
    // stack.length -= num
    stack.push(v)
    console.log("adjust f32", v, num)
    return v
}

Module.asmLibraryArg.adjustStackI64 = function (num) {
    var buffer = new ArrayBuffer(8)
    var view = new Uint8Array(buffer)
    var str = ""
    for (var i = 0; i < 8; i++) {
        view[i] = HEAP8[64+i]
        str = str + (Math.floor(view[i]/16)).toString(16) + (view[i]%16).toString(16)
    }
    console.log("adjust i64", str, num)
    stack.push(view)
    return 0
}
/*
Module.asmLibraryArg.adjustStackI64 = function (v, num) {
    // stack.length -= num
    stack.push(v)
    console.log("adjust i64", v, num)
    return v
}
*/

Module.asmLibraryArg.adjustStackF64 = function (v, num) {
    // stack.length -= num
    stack.push(v)
    console.log("adjust f64", v, num)
    return v
}

Module.asmLibraryArg.pushCritical = function (loc) {
    if (path.length == 0) return false
    console.log("at function", loc)
    fstack.push(loc)
    return startCritical(loc)
}

Module.asmLibraryArg.popCritical = function () {
    if (fstack.length > 0) fstack.length--
    step++
}

Module.asmLibraryArg.endCritical = function () {
    step++
}

Module.asmLibraryArg.startCritical = startCritical

