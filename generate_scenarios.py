#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#Author S. Thorn
#
from sklearn.model_selection import ParameterSampler
import numpy as np
from collections import OrderedDict
import math

def round_down(n, decimals=0):
    multiplier = 10 ** decimals
    return math.floor(n * multiplier) / multiplier

# Parameters (called a, b and c here) and a list of possible values for each. 
param_grid = OrderedDict({'chosenAcc': [0.5,0.7,1], \
'nBridges': [1,2,3], 'nCyclesBr': [1,2,3,4,5,6],\
'IntrogressionRate': [0.1,0.2,0.3,0.4,0.5], \
'BridgingRate': [0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5], 'ReturnRate': [0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5], \
'ExitRate': [1,2,3,4,5,6], 'ParentsPI': [50], 'GenoInPI': [6000]})


# Replicates - use all of these even if randomly sampling parameters
replicates = [1,2,3,4,5,6,7,8,9,10]

# Either choose a subset of parameters with e.g.:
#n_values = 70
# or all of them with: 
n_values = np.prod([len(param_grid[param]) for param in param_grid])

# Loop over parameters and replicates 
parameters = param_grid.keys()
for sample in ParameterSampler(param_grid, n_iter=n_values):
    if sample['ExitRate'] > sample['nCyclesBr']:
        continue

    if round_down(((sample['GenoInPI']*0.2)/(sample['nBridges']*sample['nCyclesBr'])),decimals=0) < (100-100*sample['BridgingRate']-100*sample['ReturnRate']+50):
        continue   

    if round_down(((sample['GenoInPI']*0.2)/(sample['nBridges']*sample['nCyclesBr'])),decimals=0) < (100-8-100*sample['ReturnRate']+50):
        continue
    
    if round_down(((sample['GenoInPI']*0.2)/(sample['nBridges']*sample['nCyclesBr'])),decimals=0) < (100*sample['BridgingRate']+100-100*sample['ReturnRate']) and sample['nBridges']>1 and sample['ExitRate']>1:
        continue

    if sample['nBridges']==1 and sample['BridgingRate']>0.05:
        continue

    for replicate in replicates:
        print(' '.join([str(sample[param]) for param in parameters] + [str(replicate)]))
print()        

