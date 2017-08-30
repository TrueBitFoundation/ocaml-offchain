pragma solidity ^0.4.16;

contract Instruction {
    
    bytes32[16] phases;
    address challenger;
    address prover;
    uint init;
    address winner;
    
    function Instruction(bytes32[16] arr, address c, address p) {
        phases = arr;
        challenger = c;
        prover = p;
        init = 16;
    }
    
    function select(uint q) {
        require(init == 16 && msg.sender == challenger);
        init = q;
    }
    
    struct VM {
        bytes32 code;
        bytes32 stack;
        bytes32 mem;
        bytes32 break_stack1;
        bytes32 break_stack2;
        bytes32 globals;
        bytes32 calltable;
        bytes32 call_stack;
        
        uint pc;
        uint stack_ptr;
        uint break_ptr;
        uint call_ptr;
        uint memsize;
    }
    
    VM vm;
    
    struct Machine {
        bytes32 vm;
        bytes32 op;
        uint reg1;
        uint reg2;
        uint reg3;
        uint ireg;
    }
    
    function setVM(
        bytes32 code,
        bytes32 stack,
        bytes32 mem,
        bytes32 break_stack1,
        bytes32 break_stack2,
        bytes32 globals,
        bytes32 call_stack,
        bytes32 calltable,
        
        uint pc,
        uint stack_ptr,
        uint break_ptr,
        uint call_ptr,
        uint memsize) {
        require(msg.sender == prover);
        vm.code = code;
        vm.stack = stack;
        vm.mem = mem;
        vm.call_stack = call_stack;
        vm.break_stack1 = break_stack1;
        vm.break_stack2 = break_stack2;
        vm.globals = globals;
        vm.calltable = calltable;
        vm.pc = pc;
        vm.stack_ptr = stack_ptr;
        vm.break_ptr = break_ptr;
        vm.call_ptr = call_ptr;
        vm.memsize = memsize;
    }
    
    function hashVM() returns (bytes32) {
        return sha3(vm.code, vm.stack, vm.mem, vm.call_stack, vm.break_stack1,
                    vm.break_stack2, vm.globals, vm.calltable,
                    vm.pc, vm.stack_ptr, vm.break_ptr, vm.call_ptr, vm.memsize);
    }
    
    Machine m;
    
    function setMachine(
        bytes32 vm_,
        bytes32 op,
        uint reg1,
        uint reg2,
        uint reg3,
        uint ireg) {
        m.vm = vm_;
        m.op = op;
        m.reg1 = reg1;
        m.reg2 = reg2;
        m.reg3 = reg3;
        m.ireg = ireg;
    }
    
    function hashMachine() returns (bytes32) {
        return sha3(m.vm, m.op, m.reg1, m.reg2, m.reg3, m.ireg);
    }
    
    function getLeaf(bytes32[] proof, uint loc) returns (bytes32) {
        require(proof.length >= 2);
        if (loc%2 == 0) return proof[0];
        else return proof[1];
    }
    
    function getRoot(bytes32[] proof, uint loc) returns (bytes32) {
        require(proof.length >= 2);
        bytes32 res = sha3(proof[0], proof[1]);
        loc = loc/2;
        for (uint i = 2; i < proof.length; i++) {
            if (loc%2 == 0) res = sha3(proof[i], res);
            else res = sha3(res, proof[i]);
        }
        if (loc%2 == 0) return proof[0];
        else return proof[1];
    }
    
    function proveFetch(bytes32[] proof) {
        require(init == 0 && msg.sender == prover);
        bytes32 state1 = phases[0];
        bytes32 state2 = phases[1];
        bytes32 op = getLeaf(proof, vm.pc);
        require(state1 == hashVM());
        require(state2 == sha3(state1, op));
        require(vm.code == getRoot(proof, vm.pc));
        winner = prover;
    }
    
    function getImmed(bytes32 op) internal returns (uint256) {
        // it is the first 8 bytes
        return uint(op)/(2**(24*8));
    }
    
    function proveInit(bytes32 op) {
        require(init == 1 && msg.sender == prover);
        bytes32 state1 = phases[1];
        bytes32 state2 = phases[2];
        m.vm = hashVM();
        m.op = op;
        m.reg1 = 0;
        m.reg2 = 0;
        m.reg3 = 0;
        m.ireg = getImmed(op);
        require(state1 == sha3(m.vm, op));
        require(state2 == hashMachine());
        winner = prover;
    }
    
    function readPosition(uint hint) returns (uint) {
        assert(hint > 4);
        if (hint == 5) return m.reg1;
        if (hint == 6) return vm.stack_ptr-1;
        if (hint == 7) return vm.stack_ptr-2;
        if (hint == 8) return vm.stack_ptr-1-m.reg1;
        if (hint == 9) return vm.stack_ptr-1-m.reg2;
        if (hint == 10) return vm.break_ptr-1;
        if (hint == 11) return vm.break_ptr-1;
        if (hint == 12) return vm.break_ptr-1-m.reg1;
        if (hint == 13) return vm.break_ptr-1-m.reg1;
        if (hint == 14) return vm.call_ptr-1;
        if (hint == 15) return m.reg1+m.reg2;
        if (hint == 16) return m.reg1;
    }

    function writePosition(uint hint) returns (uint) {
        assert(hint > 1);
        if (hint == 1) return m.reg1;
        if (hint == 2) return vm.call_ptr;
        if (hint == 3) return m.reg1+m.reg2;
        if (hint == 4) return vm.stack_ptr;
        if (hint == 5) return vm.stack_ptr-1;
        if (hint == 6) return vm.stack_ptr-m.reg1;
        if (hint == 7) return vm.break_ptr;
        if (hint == 8) return vm.break_ptr;
    }

    function readRoot(uint hint) returns (bytes32) {
        assert(hint > 4);
        if (hint == 5) return vm.globals;
        if (hint == 6) return vm.stack;
        if (hint == 7) return vm.stack;
        if (hint == 8) return vm.stack;
        if (hint == 9) return vm.stack;
        if (hint == 10) return vm.break_stack1;
        if (hint == 11) return vm.break_stack2;
        if (hint == 12) return vm.break_stack1;
        if (hint == 13) return vm.break_stack2;
        if (hint == 14) return vm.call_stack;
        if (hint == 15) return vm.mem;
        if (hint == 16) return vm.calltable;
    }
    
    function writeRoot(uint hint) returns (bytes32) {
        assert(hint > 1);
        if (hint == 1) return vm.globals;
        if (hint == 2) return vm.call_stack;
        if (hint == 3) return vm.mem;
        if (hint == 4) return vm.stack;
        if (hint == 5) return vm.stack;
        if (hint == 6) return vm.stack;
        if (hint == 7) return vm.break_stack1;
        if (hint == 8) return vm.break_stack2;
    }
    
    function checkReadProof(bytes32[] proof, uint loc, uint hint) returns (bool) {
        if (hint <= 4) return true;
        return readPosition(hint) == loc && readRoot(hint) == getRoot(proof, loc);
    }
    
    function checkWriteProof(bytes32[] proof, uint loc, uint hint) returns (bool) {
        if (hint == 0) return true;
        return writePosition(hint) == loc && writeRoot(hint) == getRoot(proof, loc);
    }
    
    function readFromProof(bytes32[] proof, uint loc, uint hint) returns (uint) {
        if (hint == 0) return 0;
        if (hint == 1) return m.ireg;
        if (hint == 2) return vm.pc;
        if (hint == 3) return vm.memsize;
        if (hint == 4) return vm.stack_ptr;
        return uint(getLeaf(proof, loc));
    }
    
    function proveRead1(bytes32[] proof, uint loc) {
        require(init == 2 && msg.sender == prover);
        bytes32 state1 = phases[2];
        bytes32 state2 = phases[3];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*22))&0xff;
        require(checkReadProof(proof, loc, hint));
        m.reg1 = readFromProof(proof, loc, hint);
        require(state2 == hashVM());
        winner = prover;
    }
    
    function handleALU(uint hint, uint r1, uint r2, uint r3) returns (uint) {
        return 0;
    }
    
    function proveALU() {
        require(init == 5 && msg.sender == prover);
        bytes32 state1 = phases[5];
        bytes32 state2 = phases[6];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*19))&0xff;
        m.reg1 = handleALU(hint, m.reg1, m.reg2, m.reg3);
        require(state2 == hashVM());
        winner = prover;
    }
    
    function makeChange(bytes32[] proof, uint loc, uint v) returns (bytes32) {
        assert(proof.length >= 2);
        if (loc%2 == 0) proof[0] = bytes32(v);
        else proof[1] = bytes32(v);
        return getRoot(proof, loc);
    }

    function writeStuff(uint hint, bytes32[] proof, uint loc, uint v) {
        if (hint == 0) return;
        bytes32 root = makeChange(proof, loc, v);
        if (hint == 1) vm.globals = root;
        if (hint == 2) vm.call_stack = root;
        if (hint == 3) vm.mem = root;
        if (hint == 4) vm.stack = root;
        if (hint == 5) vm.stack = root;
        if (hint == 6) vm.stack = root;
        if (hint == 7) vm.break_stack1 = root;
        if (hint == 8) vm.break_stack2 = root;
    }
    
    function proveWrite1(bytes32[] proof, uint loc) {
        require(init == 6 && msg.sender == prover);
        bytes32 state1 = phases[6];
        bytes32 state2 = phases[7];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint target = (uint(m.op)/2**(8*18))&0xff;
        uint hint = (uint(m.op)/2**(8*17))&0xff;
        require(checkWriteProof(proof, loc, hint));
        
        uint v;
        if (target == 0) v = m.reg1;
        if (target == 1) v = m.reg2;
        if (target == 2) v = m.reg3;
        writeStuff(hint, proof, loc, v);
        
        require(state2 == hashVM());
        winner = prover;
    }
    
    function proveUpdateStackPtr() {
        require(init == 8 && msg.sender == prover);
        bytes32 state1 = phases[8];
        bytes32 state2 = phases[9];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*14))&0xff;
        vm.stack_ptr = handlePointer(hint, vm.stack_ptr);
        m.mv = hashVM();
        require(state2 == hashVM());
        winner = prover;
    }
    
}
