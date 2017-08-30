pragma solidity ^0.4.16;

// "12", "13", "121212", "23232323", 123, 12, 100

contract Interactive2 {
    struct Record {
        address prover;
        address challenger;
        
        bytes32 start_state;
        bytes32 end_state;
        
        // Maybe number of steps should be finished
        uint256 steps;
        
        address winner;
        address next;
        
        uint256 size;
        uint256 timeout;
        uint256 clock;
        
        uint256 idx1;
        uint256 idx2;
        
        bytes32[] proof;
        bytes32[16] result;
    }
    
    Record[] records;
    
    function make(address p, address c, bytes32 s, bytes32 e, uint256 _steps,
        uint256 par, uint to) returns (uint) {
        records.length++;
        Record storage r = records[records.length-1];
        r.prover = p;
        r.challenger = c;
        r.start_state = s;
        r.end_state = e;
        r.steps = _steps;
        r.size = par;
        if (r.size > r.steps - 2) r.size = r.steps-2;
        r.timeout = to;
        r.clock = block.number;
        r.next = r.prover;
        r.idx1 = 0;
        r.idx2 = r.steps-1;
        r.proof.length = r.steps;
        return records.length-1;
    }

    function gameOver(uint id) {
        Record storage r = records[id];
        require(block.number >= r.clock + r.timeout);
        if (r.next == r.prover) r.winner = r.challenger;
        else r.winner = r.prover;
    }

    function report(uint id, bytes32[] arr) {
        Record storage r = records[id];
        require(r.size != 0 && arr.length == r.size &&
                msg.sender == r.prover && r.prover == r.next);
        r.clock = block.number;
        uint iter = (r.idx2-r.idx1-1)/r.size;
        for (uint i = 0; i < arr.length; i++) {
            r.proof[r.idx1+1+iter*i] = arr[i];
        }
        if (r.idx2-r.idx1-1 == 0) {
            r.size = 0;
        }
        else r.next = r.challenger;
    }

    function query(uint id, uint num) {
        Record storage r = records[id];
        require(r.size != 0 && num <= r.size &&
                msg.sender == r.challenger && r.challenger == r.next);
        r.clock = block.number;
        uint iter = (r.idx2-r.idx1-1)/r.size;
        r.idx1 = r.idx1+1+iter*num;
        r.idx2 = r.idx1+iter;
        if (r.size > r.idx2-r.idx1-1) r.size = r.idx2-r.idx1-1;
        r.next = r.prover;
    }
    
    function getStep(uint id, uint idx) returns (bytes32) {
        Record storage r = records[id];
        return r.proof[idx];
    }

    function microStep(uint id, bytes32[16] arr) {
        Record storage r = records[id];
        require(r.size == 0 && msg.sender == r.prover);
        r.result = arr;
    }
    
    function getResult(uint id) returns (bytes32[16]) {
        Record storage r = records[id];
        return r.result;
    }

}

