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


table = pd.read_csv("../data/tables/bracken_tables/" + sys.argv[1] , index_col = 0)
table = table.astype(float)

print(table)
table.index = table.index.astype(str)

#table = load_table(sys.argv[1])

#el primer argumento debería ser la tabla de abundancia

outName = sys.argv[2]
#el segundo argumento debería ser el inicio del nombre de salida para las tablas

# https://biom-format.org/documentation/table_objects.html
numTaxons = int(table.shape[0])
numSamples = int(table.shape[1])
print(numTaxons)

rawData = table
statisticsFunctions.ReBoot(rawData)
#statisticsFunctions.norm(rawData)
finish = datetime.datetime.now()
print ( f"loading: \t {finish-start}")

network = list()

        
network = statisticsFunctions.CalculateMetricsParallel(rawData)

statisticsFunctions.printNetwork(network,f"../data/networks/{outName}_raw_network.csv")

statisticsFunctions.printNetworkGephi(network,list(rawData.index),f"../data/networks/{outName}_network")

#sys.exit()

finish = datetime.datetime.now()
print ( f"raw network: \t {finish-start}")


statisticsFunctions.PermutationTest(rawData, network, numPermutations = numPermutations, reBoot = True)
finish = datetime.datetime.now()
statisticsFunctions.printNetwork(network,f"../data/networks/{outName}_network_PermTest.csv")
print ( f"PERMUTATION test: \t {finish-start}")


statisticsFunctions.PermutationTest(rawData, network, bootstrap=True, numPermutations = numPermutations, reBoot = True)
finish = datetime.datetime.now()
statisticsFunctions.printNetwork(network,f"../data/networks/{outName}_network_complete.csv")
print ( f"BOOTSTRAP test: \t {finish-start}")

