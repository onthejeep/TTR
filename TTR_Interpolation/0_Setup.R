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

GlobalLocalCluster = makeCluster(as.numeric(Config$model$core), type = 'PSOCK');
source('1_ParticalSwarm_NerualNetwork_Relax.R');

#stopCluster(GlobalLocalCluster);