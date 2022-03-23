# SHS-sim
Simulator for stochastic hybrid programs

The constructs are:

- assignment x := theta
- nd_assignment x := *
- test ?H
- SDE dx = b dt + sigma dW & H
- choice lambda a + nu b
- composition a ; b
- iteration a^*

For nd assignment - specify distribution for each variable / give function state -> distribution
SDE - try euler-maruyama first. Assume they always evolve until they reach the boundary (for now?)


