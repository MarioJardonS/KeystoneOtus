import math
#from biom import load_table
from scipy.spatial import distance
from scipy import stats
import pandas as pd
import numpy as np
import datetime
from joblib import Parallel, delayed
import statisticsFunctions
import os
import sys

# if not os.path.exists('networks'):
#     os.mkdir('networks')

np.random.seed(1)

start = datetime.datetime.now()
numPermutations = 100
numBootstraps   = 100
loadNetworkFlag = False


table_0 = pd.read_csv(sys.argv[1] , index_col = 0)
table_0 = table_0.astype(float)
table_0.index = table_0.index.astype(str)



table_1 = pd.read_csv(sys.argv[2] , index_col = 0)
table_1 = table_1.astype(float)
table_1.index = table_1.index.astype(str)



#table = load_table(sys.argv[1])

#el primer argumento debería ser la tabla de abundancia

outName = sys.argv[3]
#el segundo argumento debería ser el inicio del nombre de salida para las tablas

# https://biom-format.org/documentation/table_objects.html
numTaxons = int(table.shape[0])
numSamples = int(table.shape[1])
print(numTaxons)

rawData = table
statisticsFunctions.ReBoot(rawData)


finish = datetime.datetime.now()
print ( f"loading: \t {finish-start}")

network = list()

        
network = statisticsFunctions.CalculateMetricsParallel(rawData)

statisticsFunctions.printNetwork(network,f"../output/networks/{outName}_raw_network.csv")

statisticsFunctions.printNetworkGephi(network,list(rawData.index),f"../output/networks/{outName}_network")

#sys.exit()

finish = datetime.datetime.now()
print ( f"raw network: \t {finish-start}")


statisticsFunctions.PermutationTest(rawData, network, numPermutations = numPermutations, reBoot = True)
finish = datetime.datetime.now()
statisticsFunctions.printNetwork(network,f"../output/networks/{outName}_network_PermTest.csv")
print ( f"PERMUTATION test: \t {finish-start}")


statisticsFunctions.PermutationTest(rawData, network, bootstrap=True, numPermutations = numPermutations, reBoot = True)
finish = datetime.datetime.now()
statisticsFunctions.printNetwork(network,f"../output/networks/{outName}_network_complete.csv")
print ( f"BOOTSTRAP test: \t {finish-start}")

