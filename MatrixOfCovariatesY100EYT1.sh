#!/bin/bash
#$ -l h_vmem=1G #memory per core

#$ -cwd
#$ -R y
#$ -j y
#$ -t 10001-11295
#$ -tc 100

cd $(sed -n "${SGE_TASK_ID}p" lsOutput.txt)

export R_LIBS_USER="/home/ibreider/RLIB"
export PATH=/exports/cmvm/eddie/eb/groups/hickey_group/R-Folder/R-3.5.1/bin:$PATH
export LD_LIBRARY_PATH=/exports/eddie3_apps_local/apps/SL7/intel/parallel_studio_xe_2017_update4/compilers_and_libraries/linux/lib/intel64:/exports/eddie3_apps_local/apps/SL7/intel/parallel_studio_xe_2017_update4/compilers_and_libraries/linux/mkl/lib/intel64:$LD_LIBRARY_PATH

Rscript ../MatrixOfCovariatesY100EYT1.R

