pragma solidity ^0.4.16;

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
        
        uint256 phase;
        
        bytes32[] proof;
        bytes32[16] result;
    }
    
    Record[] records;
    
    function testMake() returns (uint) {
        return make(msg.sender, msg.sender, bytes32(123), bytes32(123),
                    10, 1, 10);
    }
    
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
        r.phase = 16;
        return records.length-1;
    }

    function gameOver(uint id) {
        Record storage r = records[id];
        require(block.number >= r.clock + r.timeout);
        if (r.next == r.prover) r.winner = r.challenger;
        else r.winner = r.prover;
    }
    
    function getIter(uint id) returns (uint it, uint i1, uint i2) {
        Record storage r = records[id];
        it = (r.idx2-r.idx1)/(r.size+1);
        i1 = r.idx1;
        i2 = r.idx2;
    }

    function report(uint id, bytes32[] arr) {
        Record storage r = records[id];
        require(r.size != 0 && arr.length == r.size &&
                msg.sender == r.prover && r.prover == r.next);
        r.clock = block.number;
        uint iter = (r.idx2-r.idx1)/(r.size+1);
        for (uint i = 0; i < arr.length; i++) {
            r.proof[r.idx1+iter*(i+1)] = arr[i];
        }
        r.next = r.challenger;
    }
    
    function testReport(uint id) returns (uint) {
        bytes32[] memory arr = new bytes32[](1);
        arr[0] = bytes32(0xffff);
        report(id, arr);
    }
    
    function roundsTest(uint rounds, uint stuff) returns (uint it, uint i1, uint i2) {
        uint id = testMake();
        for (uint i = 0; i < rounds; i++) {
            testReport(id);
            query(id, stuff % 2);
            stuff = stuff/2;
        }
        return getIter(id);
    }

    function query(uint id, uint num) {
        Record storage r = records[id];
        require(r.size != 0 && num <= r.size &&
                msg.sender == r.challenger && r.challenger == r.next);
        r.clock = block.number;
        uint iter = (r.idx2-r.idx1)/(r.size+1);
        r.idx1 = r.idx1+iter*num;
        // If last segment was selected, do not change last index
        if (num != r.size) r.idx2 = r.idx1+iter;
        if (r.size > r.idx2-r.idx1-1) r.size = r.idx2-r.idx1-1;
        // size eventually becomes zero here
        r.next = r.prover;
    }

    function getStep(uint id, uint idx) returns (bytes32) {
        Record storage r = records[id];
        return r.proof[idx];
    }

    function postPhases(uint id, bytes32[16] arr) {
        Record storage r = records[id];
        require(r.size == 0 && msg.sender == r.prover &&
                r.proof[r.idx1] == arr[0] && r.proof[r.idx1+1] == arr[15]);
        r.result = arr;
        r.next == r.challenger;
    }

    function getResult(uint id) returns (bytes32[16]) {
        Record storage r = records[id];
        return r.result;
    }
    
    function selectPhase(uint id, uint q) {
        Record storage r = records[id];
        require(r.phase == 16 && msg.sender == r.challenger);
        r.phase = q;
    }

}

