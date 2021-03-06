
#devtools::install_github("fsaer/tirefittingr")

library(plotly)
library(plyr)       #   if you use functions from any of these packages
library(dplyr)
library(magrittr)
library(tirefittingr)


fitRound7FX = function() {
  # I just run through each line within this funciton by putting the cursor
  # on it and hitting ctrl+enter
  # I put them inside functions for organization and so the lines don't run
  # if I accidentally hit run all (ctrl+shft+enter)

  #Select FX Files!!
  svFiles7FX = splitTireDataAndSave(NULL, P = c(55.2,68.9,82.7,96.5))

  clearTirefittingrOptions()
  setFXPure2002.wIA() # Change to setFYPure2002() if you want to do FY
  options(
    tirefittingr.coldCutoffTemp = 40,
    tirefittingr.iParallelCores = 5,
    tirefittingr.iEvolIterMax = NULL, #defaults to 300
    tirefittingr.iDataPoints = NULL, #defaults to 4000
    tirefittingr.sSavePlotPath = normalizePath("./plots/"),
    tirefittingr.bPlotRunConditions = TRUE)

  dfFitSummary = fitTires(svFiles7FX$FileName, svFiles7FX$RunName,
    sSummaryExportFolder = normalizePath("./plot2"))
}

fitWithoutSplitingPressure = function() {
  # I just run through each line within this funciton by putting the cursor
  # on it and hitting ctrl+enter
  # I put them inside functions for organization and so the lines don't run
  # if I accidentally hit run all (ctrl+shft+enter)

  clearTirefittingrOptions()

  options(
    tirefittingr.coldCutoffTemp = 40,
    tirefittingr.iParallelCores = 5,
    tirefittingr.iEvolIterMax = NULL, #defaults to 300
    tirefittingr.iDataPoints = NULL, #defaults to 4000
    tirefittingr.sSavePlotPath = normalizePath("C:/Users/"),
    tirefittingr.bPlotRunConditions = TRUE)

  setFXPure2002.wIA() # Change to setFYPure2002() if you want to do FY
  dfFitSummary = fitTires(
                          sSummaryExportFolder = normalizePath("C:/Users/"))

  setFYPure2002() # Change to setFYPure2002() if you want to do FY
  dfFitSummary = fitTires(
    sSummaryExportFolder = normalizePath("C:/Users/"))
}

##########################################
# The following are old and are just here for additional reference. Trying to
# understand them mau confuse you more
##########################################
fitRound8FY = function() {

  # first column has full file paths for each .dat data file
  # second column contains a name of the tire, compound, rim, pressure etc.
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

  #Do a test of the first two runs
  dfFitSummary = fitTires(
    runArray[1:2,1],
    runArray[1:2,2],
    sSummaryExportFolder = normalizePath("./plot2"))

  #Do all the runs
  dfFitSummary = fitTires(
    runArray[,1],
    runArray[,2],
    sSummaryExportFolder = normalizePath("./plot2"))
}
splitRound8FX = function() {

  # first column has full file paths for each .dat data file
  # second column contains a name of the tire, compound, rim, pressure etc.
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

  # first column, named "Base" has full file paths for each .dat data file
  # second column contains a name of the tire, compound, rim, pressure etc.
  # third coumn named "run" has "FX" "FY" or "warmup
  runArray = read.csv(choose.files(), stringsAsFactors = FALSE)

  #filter out warmup runs
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
