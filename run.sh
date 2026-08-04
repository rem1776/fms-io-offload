#!/bin/bash
pushd rundir
touch input.nml
mpirun -n 2 ../model.x : -n 1 ../writer.x
