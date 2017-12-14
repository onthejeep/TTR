library(XML)
library(ggplot2)
library(lubridate)
library(ggmap)
library(rgdal)
library(RColorBrewer)
library(RODBC);
library(RSQLite)
library(parallel)

setwd('D:/MySVN/UA-Research/Dr Xia/R Code/TTR_Interpolation/TTR_Interpolation');

Config = xmlToList(xmlParse('Config/GlobalParas.xml'));
# Config = xmlToList(xmlParse('Config/GlobalParas_Relax.xml'));
print(sprintf('Using %s cores to train ... ', Config$model$core$text));
print(sprintf('%s solutions in a generation', Config$model$neuralnetwork$numsolution$text));
print(sprintf('Neural network structure is %s', Config$model$neuralnetwork$structure$text));
print(sprintf('%s times of bagging', Config$model$bagging$numBootstrap$text));

GlobalLocalCluster = makeCluster(as.numeric(Config$model$core), type = 'PSOCK', output = 'a.txt');
#source('1_ParticalSwarm_NerualNetwork_Relax.R');
source('1_ParticalSwarm_NerualNetwork.R');

#stopCluster(GlobalLocalCluster);