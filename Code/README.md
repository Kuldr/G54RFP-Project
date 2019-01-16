All code in this project was written from scratch

The source code is split in to separate modules CNF and Solvers.
The CNF module handles problem generation and evaluation.
The Solvers module contains a submodule for each type of solver as well as a file with common functions.
testSolvers.hs is a simple way to check a solver was working.
EvaluateSolvers.hs is a program that can be compiled to compare the performance of the different solvers.
Below shows a diagram of the folder hierarchy

.
├── CNF
│   ├── Evaluator.hs
│   ├── Generator.hs
│   └── Types.hs
├── Solvers
│   ├── Common.hs
│   ├── GeneticAlgorithm.hs
│   ├── HillClimbing.hs
│   └── Naive.hs
├── README.md
└── testSolvers.hs
