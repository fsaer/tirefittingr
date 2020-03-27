
#devtools::install_github("fsaer/tirefittingr")

library(plotly)
library(plyr)       #   if you use functions from any of these packages
library(dplyr)
library(magrittr)
library(tirefittingr)


fitRound8FY = function() {
path1 = "C:\\Users\\jim\\Documents\\R\\FittingTires\\listToFit.csv"
runArray = read.csv(path1, stringsAsFactors = FALSE)

clearTirefittingrOptions()
setFXPure2002()

if (TRUE) {
  options(
    tirefittingr.coldCutoffTemp = 40,
    tirefittingr.iParallelCores = 5,
    tirefittingr.iEvolIterMax = NULL, 
    tirefittingr.iDataPoints = NULL,
    tirefittingr.sSavePlotPath = normalizePath("./plots/"),
    tirefittingr.bPlotRunConditions = TRUE)
} else {
  options(
    tirefittingr.iParallelCores = 1,
    tirefittingr.iEvolIterMax = 30, 
    tirefittingr.iDataPoints = NULL,
    tirefittingr.sSavePlotPath = normalizePath("./plots/"),
    tirefittingr.bPlotRunConditions = TRUE)
}


dfFitSummary = fitTires(
  runArray[1:2,1],
  runArray[1:2,2],
  sSummaryExportFolder = normalizePath("./plot2"))
}

splitRound8FX = function() {
  svFiles = read.csv(choose.files(), stringsAsFactors = F)[,1]
  splitTireDataAndSave(svFiles, P = c(55.2,68.9,82.7,96.5))

}
  

fitRound8FX = function() {
  
  clearTirefittingrOptions()
  setFXPure2002.wIA()
  options(
    tirefittingr.coldCutoffTemp = 40,
    tirefittingr.iParallelCores = 5,
    tirefittingr.iEvolIterMax = NULL, 
    tirefittingr.iDataPoints = NULL,
    tirefittingr.sSavePlotPath = normalizePath("./plots/"),
    tirefittingr.bPlotRunConditions = TRUE)

  dfFitSummary = fitTires(
    sSummaryExportFolder = normalizePath("./plot2"))
  
}

fitRound6FY = function() {
  runArray = read.csv(choose.files(), stringsAsFactors = FALSE)
  
  runArray2 = dplyr::filter(runArray, Run != "warmup")
  
  uniqueRuns = unique(runArray2$Base)
  
  toSplit = paste("D:/BACKUP SYNCS/FSAE/TTC Data/RunData/", 
                uniqueRuns, sep = "")
  
    
  temp = splitTireDataAndSave(toSplit, P = 6.894*c(8,10,12,14))
  
  clearTirefittingrOptions()
  setFyPure2002()
  
  options(
    tirefittingr.coldCutoffTemp = 40,
    tirefittingr.iParallelCores = 4,
    tirefittingr.iEvolIterMax = NULL, 
    tirefittingr.iDataPoints = NULL,
    tirefittingr.sSavePlotPath = normalizePath("./plots/"),
    tirefittingr.bPlotRunConditions = TRUE)

  dfFitSummary = fitTires(
    temp$FileName[1:44],
    temp$RunName[1:44],
    sSummaryExportFolder = normalizePath("./plot2"))

  setFXPure2002.wIA()
  dfFitSummary = fitTires(
    temp$FileName[45:88],
    temp$RunName[45:88],
    sSummaryExportFolder = normalizePath("./plot2"))
  
    
}

