# MultiPartBreedingStrategy
Scripts used for "A multipart strategy for introgression of exotic germplasm into elite plant breeding programs using genomic selection".
#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#

Pipeline of analyses
Run Burn-in
Run BashBurnIn.sh.
This calls BurnInRwrapper.R, which calls 
CreateParentsExPVPs.R #creates founders
BurnInEliteExPVPs.R #runs initial breeding (50 years)
BurnInExoticExPVPsFirst.R #runs initial breeding (30 years)
BurnInExoticExPVPsSecond.R #runs initial breeding (30 years)
BurnInExoticExPVPsThird.R #runs initial breeding (30 years)
BurnInExoticExPVPsFourth.R #runs initial breeding (30 years)
This creates founders, elite breeding program and four exPVP programs.

Run future breeding
Use generate_scenarios.py to create input file with all multi-part strategy scenarios of interest. This script filters out scenarios that are not viable due to budget restrictions.
Run python generate_scenarios.py>Scenarios.txt

Run BashMultiPartAccSet.sh in same folder as burn-in output is located, calls Scenarios.txt and MultiPartPilarWithSetAcc.R.

Results are saved separate for each scenario in a folder named after the scenario. All results for this scenario are saved here. There are 2 output files per repeat (10 repeats) for each scenario, one with relevant populations saved for further analyses and one with only the results saved. 

Results are combined into 1 file for each scenario using StartReadOverFolders.sh.
StartReadOverFolders.sh is run in the main folder containing the subfolders for each scenario, calls lsOutput.txt, where lsOutput.txt results from ls -d */ >> lsOutput.txt (run in the main folder, assuming no subfolders other than the scenario results are in here).
StartReadOverFolders.sh calls ReadingInUniversal.R, which produces 1 file per scenario with results of all repeats merged.

To obtain the scenario resulting in the highest genetic gain a database of all results over scenarios is created (based on mean performance of each scenario over 10 repeats), using ReadScenariosMaxGenGain.sh, which calls Make_database_of_results_from_scenarios.R and lsOutput. This results in SummaryofResultsScenarios.txt, a table with values used for the parameters and output achieved under these values.

R software is used to sort SummaryofResultsScenarios.txt by genetic gain achieved to obtain the parameter values under which the multipart reaches the highest genetic gain (GetTopScenarios.R).

Above steps are also performed to obtain baselines, where variables not applicable to the two-part are set to 0. This results in 15 baselines.
Run BashMultiPartAccSetBase.sh in same folder as burn-in output is located, calls ScenariosBase.txt and MultiPartPilarWithSetAccBase.R.

Graphs
Graphs were created using
GraphsAcc.R
GraphsIR.R
GraphsnCyc.R
GraphsOptimal.R  
Using the output files of ReadingInUniversal.R of the relevant scenarios and baselines.

In depth analyses
Regression tree analysis
Create table with covariates and results: 
Run MatrixOfCovariatesY100EYT1.sh in the main folder where all results folders are, calls lsOutput.txt and MatrixOFCovariatesY1000EYT1.R. 
Creates MatrixOfCovariatesY100EYT1.txt

Create table with covariates and results baselines:
Run MatrixOfCovariatesY100EYT1Base.sh in the folder where all baseline results folders are, calls lsOutput.txt and MatrixOFCovariatesY1000EYT1Base.R.
Creates MatrixOfCovariatesY100EYT1Base.txt. Manually Baselines with nCyclesBr=1 need to be added at same values as nCyclesBr=2 as they are the baseline for the multipart with nCyclesBr=1 in bridges.

Create regression tree with RegTree.R.

Haploblock effect analysis
Perform analyses in folder where the full output files (10 reps) of the scenario of interest are located.
Run BashHaploBlockAnalysesStartOfPI.sh, calls HaploBlockTestInputFile.txt and HaploBlockAnalysesStartOfPI.R. Results in .rda files, which can be merged and accessed with HaploMergeEffectTables.R.

Haploblock origin analysis
Perform analyses in folder where the full output files (10 reps) of the scenario of interest are located.
Run HaploblockOriginPullY100.sh to obtain haplotypes, calls HaploblockOriginPullY100.R.
Run BashHaploBlockOriginY100.sh, calls HaploBlockOriginY100.R. and .rda files resulting from HaploblockOriginPullY100.R.
Results in .rda files, which are input for PieChart.R. 

