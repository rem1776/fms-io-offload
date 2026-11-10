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

# 1-6 nodes worth of pes
#offload_pes="126 252 384 510 636 768"
offload_pes="6"
offload_npes="6"
model_npes=$(($layoutx * $layouty * 6))

# how many writes can be pending while still receiving new operations
write_cache_size=512

base_dir=${layoutx}x${layouty}_c${gridsize}_offload_${write_cache_size}_cache
mkdir -p ${base_dir} 
cd ${base_dir}

cat <<_EOF > input.nml 
&offloading_io_nml
max_pending_writes=${write_cache_size}
/

&model_driver_nml
nx=$gridsize,
ny=$gridsize,
nz=65,
nxhalo=${nhalo},
nyhalo=${nhalo},
nwriters=$offload_npes,
domain_type=2, ! cubed sphere domain
layout=${layoutx},${layouty}
/

&writer_driver_nml
nwriters=$offload_npes
/
_EOF

total_npes=$(( $model_npes + $offload_npes ))

cat <<_EOF > run.sh
#!/bin/sh -fx
#FRE scheduler-options
#SBATCH --output=/gpfs/f5/gfdl_f/scratch/Ryan.Mulhall/fms-io-offload/stdout/${layoutx}x${layouty}_c${gridsize}_${offload_npes}pes_${write_cache_size}_cache_offload.out.%j
#SBATCH --job-name=${layoutx}x${layouty}_c${gridsize}_${offload_npes}pes_offload
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

export LD_LIBRARY_PATH="/gpfs/f5/gfdl_f/scratch/Ryan.Mulhall/fms-io-offload/fms/${builddir}/libFMS/.libs:${LD_LIBRARY_PATH}"

time srun -n ${total_npes} --multi-prog multi-executable-config.conf
_EOF

last_model_pe=$(( $model_npes - 1 ))
last_writer_pe=$(( $total_npes - 1 ))

cat <<_EOF > multi-executable-config.conf
0-${last_model_pe}	../../offload_model_driver.x
${model_npes}-${last_writer_pe}	../../offload_writer_driver.x
_EOF

test "$3" = "--submit" && sbatch run.sh 

