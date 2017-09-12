pragma solidity ^0.4.16;

contract Tasks {
    
    event Posted(address giver, bytes32 hash, string file, uint id);
    event Solved(uint id, bytes32 hash, uint steps, bytes32 init, string file);
    
    struct Task {
        address giver;
        bytes32 init;
        string file;
        
        address solver;
        bytes32 result;
        uint steps;
    }
    
    Task[] public tasks;
    
    function add(bytes32 init, string file) {
        tasks.push(Task(msg.sender, init, file, 0, 0, 0));
        Posted(msg.sender, init, file, tasks.length-1);
    }
    
    function solve(uint id, bytes32 result, uint steps) {
        require(tasks[id].solver == 0);
        tasks[id].solver = msg.sender;
        tasks[id].result = result;
        tasks[id].steps = steps;
        Solved(id, result, steps, tasks[id].init, tasks[id].file);
    }

}

