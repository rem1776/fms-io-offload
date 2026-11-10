#!/bin/bash
# usage: ./script.sh xlayout ylayout [--submit]

echo args: $#
test $# -ge 2 || echo "Needs layout as arguments"
test $# -ge 2 || exit 1

layoutx=$1
layouty=$2
gridsize=384

nhalo=0

builddir="build"

model_npes=$(($layoutx * $layouty * 6))

base_dir=${layoutx}x${layouty}_c${gridsize}_baseline
mkdir -p ${base_dir} 
cd ${base_dir}

cat <<_EOF > input.nml 
&test_io_offloading_nml
nx=$gridsize,
ny=$gridsize,
nz=65,
nxhalo=${nhalo},
nyhalo=${nhalo},
domain_type=2, ! cubed sphere domain
layout=${layoutx},${layouty}
/
_EOF

total_npes=$model_npes

cat <<_EOF > run.sh
#!/bin/sh -fx
#FRE scheduler-options
#SBATCH --output=/gpfs/f5/gfdl_f/scratch/Ryan.Mulhall/io_offload_pt2/stdout/${layoutx}x${layouty}_c${gridsize}_baseline.out.%j
#SBATCH --job-name=${layoutx}x${layouty}_c${gridsize}_baseline
#SBATCH --time=180
#SBATCH --qos=normal
#SBATCH --partition=batch
#SBATCH --clusters=c5
#SBATCH --ntasks=${total_npes}
#SBATCH --mail-user=Ryan.Mulhall@noaa.gov
#SBATCH --mail-type=FAIL
#SBATCH --account=gfdl_f

module load intel-classic/2023.2.0
module unload cray-libsci

ulimit -s unlimited

export FI_VERBS_PREFER_XRC=0

export LD_LIBRARY_PATH="/gpfs/f5/gfdl_f/scratch/Ryan.Mulhall/io_offload_pt2/fms/${builddir}/libFMS/.libs:${LD_LIBRARY_PATH}"

time srun -n ${total_npes} ../../baseline_test_driver.x 
_EOF

test "$3" = "--submit" && sbatch run.sh 

