library(parallel)



GlobalLocalCluster = makeCluster(6, type = 'PSOCK'); #, outfile = 'a.txt'

source('1_ParticalSwarm_NerualNetwork_Relax.R');

#stopCluster(GlobalLocalCluster);