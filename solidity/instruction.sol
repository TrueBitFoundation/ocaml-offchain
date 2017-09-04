pragma solidity ^0.4.15;

import "./alu.sol";

contract Instruction {
    
    bytes32[14] phases;
    address challenger;
    address prover;
    uint init;
    address winner;
    
    function Instruction(bytes32[14] arr, address c, address p) {
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
        return sha3(vm.code, vm.mem, vm.stack, vm.globals, vm.call_stack, vm.break_stack1,
                    vm.break_stack2, vm.calltable,
                    vm.pc, vm.stack_ptr, vm.call_ptr, vm.break_ptr, vm.memsize);
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
        for (uint i = 2; i < proof.length; i++) {
            loc = loc/2;
            if (loc%2 == 0) res = sha3(res, proof[i]);
            else res = sha3(proof[i], res);
        }
        return res;
    }

    function proveFetch(bytes32[] proof) returns (bool) {
        require(init == 0 && msg.sender == prover);
        bytes32 state1 = phases[0];
        bytes32 state2 = phases[1];
        bytes32 op = getLeaf(proof, vm.pc);
        require(state1 == hashVM());
        require(state2 == sha3(state1, op));
        require(vm.code == getRoot(proof, vm.pc));
        winner = prover;
        return true;
    }

    function getImmed(bytes32 op) internal returns (uint256) {
        // it is the first 8 bytes
        return uint(op)/(2**(13*8));
    }

    function proveInit(bytes32 op) returns (bool) {
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
        return true;
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
        if (hint == 1) return vm.break_ptr;
        if (hint == 2) return vm.stack_ptr-m.reg1;
        if (hint == 3) return vm.stack_ptr;
        if (hint == 4) return vm.stack_ptr-1;
        if (hint == 5) return m.reg1+m.reg2;
        if (hint == 6) return vm.call_ptr;
        if (hint == 7) return vm.break_ptr;
        if (hint == 8) return m.reg1;
        if (hint == 9) return vm.stack_ptr-2;
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
        if (hint == 1) return vm.break_stack1;
        if (hint == 2) return vm.stack;
        if (hint == 3) return vm.stack;
        if (hint == 4) return vm.stack;
        if (hint == 5) return vm.mem;
        if (hint == 6) return vm.call_stack;
        if (hint == 7) return vm.break_stack2;
        if (hint == 8) return vm.globals;
        if (hint == 9) return vm.stack;
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
        if (hint == 2) return vm.pc+1;
        if (hint == 3) return vm.memsize;
        if (hint == 4) return vm.stack_ptr;
        return uint(getLeaf(proof, loc));
    }
    
    function proveRead1(bytes32[] proof, uint loc) returns (bool) {
        require(init == 2 && msg.sender == prover);
        bytes32 state1 = phases[2];
        bytes32 state2 = phases[3];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*0))&0xff;
        require(checkReadProof(proof, loc, hint));
        m.reg1 = readFromProof(proof, loc, hint);
        require(state2 == hashMachine());
        winner = prover;
        return true;
    }
    function proveRead2(bytes32[] proof, uint loc) returns (bool) {
        require(init == 3 && msg.sender == prover);
        bytes32 state1 = phases[3];
        bytes32 state2 = phases[4];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*1))&0xff;
        require(checkReadProof(proof, loc, hint));
        m.reg2 = readFromProof(proof, loc, hint);
        require(state2 == hashMachine());
        winner = prover;
        return true;
    }
    function proveRead3(bytes32[] proof, uint loc) returns (bool) {
        require(init == 4 && msg.sender == prover);
        bytes32 state1 = phases[4];
        bytes32 state2 = phases[5];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*2))&0xff;
        require(checkReadProof(proof, loc, hint));
        m.reg3 = readFromProof(proof, loc, hint);
        require(state2 == hashMachine());
        winner = prover;
        return true;
    }
    function handleALU(uint hint, uint r1, uint r2, uint r3) returns (uint) {
        uint res;
        if (hint == 0) return r1;
        else if (hint == 1) revert(); // Trap
        else if (hint == 2) {
            if (r1 < r2) res = r1;
            else res = r2;
        }
        else if (hint == 3) {
            if (r1 == 0) res = r2;
            else res = r3;
        }
        else if (hint == 0x45 || hint == 0x50) {
            if (r1 == 0) res = 1;
            else res = 0;
        }
        else if (hint == 0x46 || hint == 0x51) {
            if (r1 == r2) res = 1;
            else res = 0;
        }
        else if (hint == 0x47 || hint == 0x52) {
            if (r1 == r2) res = 1;
            else res = 0;
        }
        else if (hint == 0x48) {
            if (int32(r1) < int32(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x49) {
            if (uint32(r1) < uint32(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x4a) {
            if (int32(r1) > int32(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x4b) {
            if (uint32(r1) > uint32(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x4c) {
            if (int32(r1) <= int32(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x4d) {
            if (uint32(r1) <= uint32(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x4e) {
            if (int32(r1) >= int32(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x4f) {
            if (uint32(r1) >= uint32(r2)) res = 0;
            else res = 1;
        }

        else if (hint == 0x53) {
            if (int64(r1) < int64(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x54) {
            if (uint64(r1) < uint64(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x55) {
            if (int64(r1) > int64(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x56) {
            if (uint64(r1) > uint64(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x57) {
            if (int64(r1) <= int64(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x58) {
            if (uint64(r1) <= uint64(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x59) {
            if (int64(r1) >= int64(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x5a) {
            if (uint64(r1) >= uint64(r2)) res = 0;
            else res = 1;
        }
        else if (hint == 0x67) {
            // clz: count leading zeros 32
        }
        else if (hint == 0x68) {
            // ctz: count trailing zeros 32
        }
        else if (hint == 0x69) {
            // popcnt: count ones 32
        }
        else if (hint == 0x79) {
            // clz: count leading zeros 64
        }
        else if (hint == 0x7a) {
            // ctz: count trailing zeros 64
        }
        else if (hint == 0x7b) {
            // popcnt: count ones 64
        }
        else if (hint == 0x6a || hint == 0x7c) {
            res = r1+r2;
        }
        else if (hint == 0x6b || hint == 0x7d) {
            res = r1-r2;
        }
        else if (hint == 0x6c || hint == 0x7e) {
            res = r1*r2;
        }
        else if (hint == 0x6d) {
            res = uint(int32(r1)/int32(r2));
        }
        else if (hint == 0x7f) {
            res = uint(int64(r1)/int64(r2));
        }
        else if (hint == 0x6e || hint == 0x80) {
            res = r1/r2;
        }
        else if (hint == 0x6f) {
            res = uint(int32(r1)%int32(r2));
        }
        else if (hint == 0x81) {
            res = uint(int64(r1)%int64(r2));
        }
        else if (hint == 0x70 || hint == 0x82) {
            res = r1%r2;
        }
        else if (hint == 0x71 || hint == 0x83) {
            res = r1&r2;
        }
        else if (hint == 0x72 || hint == 0x84) {
            res = r1|r2;
        }
        else if (hint == 0x73 || hint == 0x85) {
            res = r1^r2;
        }
        else if (hint == 0x74 || hint == 0x86) {
            res = r1*2**r2; // shift 
        }
        else if (hint == 0x75 || hint == 0x87) {
            res = r1/2**r2;
        }
        else if (hint == 0x76 || hint == 0x88) {
            res = r1/2**r2;
        }
        // rol, ror
        else if (hint == 0x77) {
            res = (r1*2**r2) | (r1/2**32);
        }
        else if (hint == 0x78) {
            res = (r1/2**r2) | (r1*2**32);
        }
        else if (hint == 0x89) {
            res = (r1*2**r2) | (r1/2**64);
        }
        else if (hint == 0x8a) {
            res = (r1/2**r2) | (r1*2**64);
        }
        
        if (hint >= 0x62 && hint <= 0x78) {
            res = res % (2**32);
        }
        else if (hint >= 0x7c && hint <= 0x8a) {
            res = res % (2**64);
        }
        
        return res;
    }
    
    function proveALU() returns (bool) {
        require(init == 5 && msg.sender == prover);
        bytes32 state1 = phases[5];
        bytes32 state2 = phases[6];
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*3))&0xff;
        // m.reg1 = ALU.handleALU(hint, m.reg1, m.reg2, m.reg3);
        m.reg1 = handleALU(hint, m.reg1, m.reg2, m.reg3);
        require(state2 == hashMachine());
        winner = prover;
        return true;
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
        if (hint == 1) vm.break_stack1 = root;
        if (hint == 2) vm.stack = root;
        if (hint == 3) vm.stack = root;
        if (hint == 4) vm.stack = root;
        if (hint == 5) vm.mem = root;
        if (hint == 6) vm.call_stack = root;
        if (hint == 7) vm.break_stack2 = root;
        if (hint == 8) vm.globals = root;
        if (hint == 9) vm.stack = root;
    }
    
    function proveWrite1(bytes32[] proof, uint loc) returns (uint) {
        require(init == 6 && msg.sender == prover);
        bytes32 state1 = phases[6];
        bytes32 state2 = phases[7];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint target = (uint(m.op)/2**(8*4))&0xff;
        uint hint = (uint(m.op)/2**(8*5))&0xff;
        require(checkWriteProof(proof, loc, hint));
        
        uint v;
        if (target == 1) v = m.reg1;
        if (target == 2) v = m.reg2;
        if (target == 3) v = m.reg3;
        writeStuff(hint, proof, loc, v);
        
        m.vm = hashVM();
        require(state2 == hashMachine());
        winner = prover;
        return hint;
    }
    function proveWrite2(bytes32[] proof, uint loc) returns (bool) {
        require(init == 7 && msg.sender == prover);
        bytes32 state1 = phases[7];
        bytes32 state2 = phases[8];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint target = (uint(m.op)/2**(8*6))&0xff;
        uint hint = (uint(m.op)/2**(8*7))&0xff;
        require(checkWriteProof(proof, loc, hint));
        
        uint v;
        if (target == 1) v = m.reg1;
        if (target == 2) v = m.reg2;
        if (target == 3) v = m.reg3;
        writeStuff(hint, proof, loc, v);
        
        m.vm = hashVM();
        require(state2 == hashMachine());
        winner = prover;
        return true;
    }
    
    function handlePointer(uint hint, uint ptr) returns (uint) {
        if (hint == 0) return ptr - m.reg1;
        if (hint == 1) return m.reg1;
        if (hint == 2) return m.reg2;
        if (hint == 3) return m.reg3;
        if (hint == 4) return ptr+1;
        if (hint == 5) return ptr-1;
        if (hint == 6) return ptr;
    }

    function proveUpdatePC() returns (bool) {
        require(init == 8 && msg.sender == prover);
        bytes32 state1 = phases[8];
        bytes32 state2 = phases[9];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*11))&0xff;
        vm.pc = handlePointer(hint, vm.pc);
        m.vm = hashVM();
        require(state2 == hashMachine());
        winner = prover;
        return true;
    }
    function proveUpdateBreakPtr() returns (bool) {
        require(init == 9 && msg.sender == prover);
        bytes32 state1 = phases[9];
        bytes32 state2 = phases[10];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*10))&0xff;
        vm.break_ptr = handlePointer(hint, vm.break_ptr);
        m.vm = hashVM();
        require(state2 == hashMachine());
        winner = prover;
        return true;
    }
    function proveUpdateStackPtr() returns (bool) {
        require(init == 10 && msg.sender == prover);
        bytes32 state1 = phases[10];
        bytes32 state2 = phases[11];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*9))&0xff;
        vm.stack_ptr = handlePointer(hint, vm.stack_ptr);
        m.vm = hashVM();
        require(state2 == hashMachine());
        winner = prover;
        return true;
    }
    function proveUpdateCallPtr() returns (bool) {
        require(init == 11 && msg.sender == prover);
        bytes32 state1 = phases[11];
        bytes32 state2 = phases[12];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*8))&0xff;
        vm.call_ptr = handlePointer(hint, vm.call_ptr);
        m.vm = hashVM();
        require(state2 == hashMachine());
        winner = prover;
        return true;
    }
    function proveUpdateMemsize() returns (bool) {
        require(init == 12 && msg.sender == prover);
        bytes32 state1 = phases[12];
        bytes32 state2 = phases[13];
        require(m.vm == hashVM());
        require(state1 == hashMachine());
        uint hint = (uint(m.op)/2**(8*12))&0xff;
        if (hint == 1) vm.memsize = vm.memsize+m.reg1;
        m.vm = hashVM();
        require(state2 == hashVM());
        winner = prover;
        return true;
    }
    
}
