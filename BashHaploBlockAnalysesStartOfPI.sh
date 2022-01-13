#!/bin/bash
#!/exports/cmvm/eddie/eb/groups/tier2_hickey_group/Programs/R-4.0.0/bin/Rscript
#$ -pe sharedmem 2  #nr of cores
#$ -l h_vmem=48G #memory per core
#$ -l h_rt=02:00:00
#$ -l hostname='!tc0[1234]&!node1f01.ecdf.ed.ac.uk&!node1f02.ecdf.ed.ac.uk&!node2j01.ecdf.ed.ac.uk&!node2j02.ecdf.ed.ac.uk&!node2j03.ecdf.ed.ac.uk&!node2j04.ecdf.ed.ac.uk&!node1c08.ecdf.ed.ac.uk'

#$ -cwd
#$ -R y
#$ -j y
#$ -t 11-20
#$ -P roslin_HighlanderLab

export R_LIBS_USER="/home/ibreider/RLIB"
export PATH=/exports/cmvm/eddie/eb/groups/tier2_hickey_group/Programs/R-4.0.0/bin:$PATH
export LD_LIBRARY_PATH=/exports/eddie3_apps_local/apps/SL7/intel/parallel_studio_xe_2017_update4/compilers_and_libraries/linux/lib/intel64:/exports/eddie3_apps_local/apps/SL7/intel/parallel_studio_xe_2017_update4/compilers_and_libraries/linux/mkl/lib/intel64:$LD_LIBRARY_PATH


Rscript HaploBlockAnalysesStartOfPI.R $(sed -n "${SGE_TASK_ID}p" HaploBlockTestInputFile.txt)
