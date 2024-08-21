#!/bin/bash
elmerf90 -o PySpiceCouplerSolver.so PySpiceCouplerSolver.F90 -lprecice
# elmerf90 -o VICoupler_Solver.so VICoupler_Solver.F90 -lprecice -ljsonfortran -I/usr/local/jsonfortran-gnu-9.0.1/lib/ 