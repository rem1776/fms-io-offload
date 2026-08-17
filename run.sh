#!/bin/bash
export LD_LIBRARY_PATH+=":/home/Ryan.Mulhall/fms-io-offload/fms/build/libFMS/.libs"
mkdir -p rundir
pushd rundir
touch input.nml
rm *.nc
mpirun -n 2 ../model.x : -n 1 ../writer.x
