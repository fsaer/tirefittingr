
if (FALSE){

library(usethis)

#Not included
fLOESSFitting = function(){

    sInputFile = choose_files(
        default = paste(sRSourcePath,"*",sep = ""),
        caption = "Select a single run .csv/.dat file")

    readData = readData(sInputFile)
    #reduces the size of the dataset to speed up optimization
    dfData = dplyr::sample_n(readData,5999,replace = FALSE)

    myLOESS = loess(FY~SA+FZ+IA, data = dfData, span = 0.01)

    dfPredictA = dplyr::select(dfData,c("SA","IA","FZ"))
    dfPredictB = dplyr::select(dfData,c("SA","IA","FZ","FY"))

    vPredictFY = predict(myLOESS,dfPredictA)

    dfPredictB$FYPredict = vPredictFY


    plotA = plot_ly(
        dfPredictB,x = ~SA,y = ~FY,type = 'scatter', mode = 'markers')
    plotA = add_markers(
        plotA,x = ~SA,y = ~FYPredict, data = dfPredictB,
        marker = list(color = 'red', size = 3))
    show(plotA)
}

fRandomSample = function(data, iNumberOfDataPoints, seed = 2017){
    # UNUSED!!
    set.seed(seed)
    dfPlotData = dplyr::sample_n(
        data,
        min(iNumberOfDataPoints,
            length(data[,1])),
        replace = FALSE)
    set.seed()

    return(dfPlotData)
}

#Initialize and setup optimze function
fCreateInitialPopulation <- function(dfStartParameters, iPopulation = 200){

    if (nrow(dfStartParameters) == 0) {
        stop("Passed empty dfStartParameters to fOptimDEInitialize")
    }

    #mInitialPop<-matrix(dfStartParameters,nrow=iPopulation,ncol=length(dfStartParameters),byrow=TRUE)
    mInitialPop <- data.matrix(dfStartParameters)

    iRowDiff <- iPopulation - nrow(mInitialPop)
    while (iRowDiff > 0) {
        #recycle rows to increase mInitialPop to iPopulation number of rows
        mInitialPop <- rbind(mInitialPop,mInitialPop)
        #mInitialPop<-matrix(mInitialPop,nrow=iPopulation)
        iRowDiff <- iPopulation - nrow(mInitialPop)
    }

    rownames(mInitialPop) <- (1:length(mInitialPop[,1]))  #rbind screws up the row names, so this line fixes it

    mInitialPop <- mInitialPop[1:iPopulation,]      #reduce the number of rows in mInitialPop down to iPopulation number of rows

    #randomize the inital values a little bit for genetic variation.
    mInitialPop <- plyr::aaply(mInitialPop,c(1,2),function(x) {x*rnorm(1,1,0.1)})

    return(mInitialPop)
}

#examples, playing around, etc.

createFakeData = function() {
    library(plotly)
    # 1. make a data frame
    # 2. Write to a fake .dat file
    # 3. Read the fake .dat file and save the file for future testing.

    SummaryABC = utils::read.table("./inst/extdata/FakeCoef.csv",
               header = TRUE, sep = ",")

    ABCrun1LatPreProccessed = createFitDataFrame(FYPurePacejka2002, "parameters",
             SA = seq(-12.1,12,0.1), #starts at 12.1 so 1st row can be dropped.
             IA = c(0,4),
             FZ = c(-250, -750, -1500),
             parameters = SummaryABC[2, 9:ncol(SummaryABC)])

    #remove first column, so that data frame matches a readTireData output.
    ABCrun1LatPreProccessed = ABCrun1LatPreProccessed[-1,]

    ABCrun1LatPreProccessed$TSTC = 55
    ABCrun1LatPreProccessed$FX = 0
    ABCrun1LatPreProccessed$SR = 0
    ABCrun1LatPreProccessed$P = 55.2

    ABCrun2LatPreProccessed = ABCrun1LatPreProccessed
    ABCrun2LatPreProccessed$FY = 1.1*ABCrun2LatPreProccessed$FY
    ABCrun2LatPreProccessed$P = 82.7

    # ABCrun3LatPreProcessed = ABCrun2LatPreProccessed
    # ABCrun3LatPreProccessed$FY = 1.2*ABCrun2LatPreProccessed$FY

    usethis::use_data(ABCrun1LatPreProccessed, overwrite = TRUE)
    usethis::use_data(ABCrun2LatPreProccessed, overwrite = TRUE)

    ABCrun1LongPreProccessed = createFitDataFrame(
         FXPurePacejka2002.wIA, "parameters",
         SL = seq(-0.25, 0.25, 0.005),
         IA = c(0,4),
         FZ = c(-250, -750, -1500),
         parameters = SummaryABC[1, 9:ncol(SummaryABC)])

    ABCrun1LongPreProccessed$SA = 0
    ABCrun1LongPreProccessed$TSTC = 55
    ABCrun1LongPreProccessed$FY = 0
    ABCrun1LongPreProccessed$P = 55.2

    ABCrun2LongPreProccessed = ABCrun1LongPreProccessed
    ABCrun2LongPreProccessed$P = 82.7
    ABCrun2LongPreProccessed$FX = 1.15 * ABCrun2LongPreProccessed$FX

    usethis::use_data(ABCrun1LongPreProccessed, overwrite = T)
    usethis::use_data(ABCrun2LongPreProccessed, overwrite = T)


    writeFileWithHeader2(
        rbind(ABCrun1LatPreProccessed, ABCrun2LatPreProccessed),
        "./inst/extdata/ABCrunLat.dat")
    writeFileWithHeader2(ABCrun1LatPreProccessed, "./inst/extdata/ABCrun1Lat.dat")
    writeFileWithHeader2(ABCrun2LatPreProccessed, "./inst/extdata/ABCrun2Lat.dat")
    writeFileWithHeader2(ABCrun1LongPreProccessed, "./inst/extdata/ABCrun1Long.dat")
    writeFileWithHeader2(ABCrun2LongPreProccessed, "./inst/extdata/ABCrun2Long.dat")

    #warning("open and manually edit to add title, and units rows before utils::write.table")
    #utils::write.table(ABCrun1LatPreProccessed, "./data/ABCrun1Lat.dat", sep = "\t")
    #utils::write.table(ABCrun2LatPreProccessed, "./data/ABCrun2Lat.dat", sep = "\t")

    #edit to add in columns before
    #warning("open and manually edit to add title, and units rows before utils::write.table")
    # utils::write.table(ABCrun1LongPreProccessed, "./data/ABCrun1Long.dat", sep = "\t")
    # utils::write.table(ABCrun2LongPreProccessed, "./data/ABCrun2Long.dat", sep = "\t")
}

# Internal function for generating sample data for examples
writeFileWithHeader2 = function(x, file) {
    unlink(file) #deletes existing file
    fil = file(file)
    con = open.connection(fil, open = "wt+at")
    writeLines("ABC Data", fil)
    close.connection(fil)
    fil = file(file)
    con = open.connection(fil, open = "wt+at")
    cat("ABC Data", sep = "\t", file = fil, append = TRUE)
    writeLines("", fil)
    cat(colnames(x),
        sep = "\t", file = fil, append = TRUE)
    writeLines("", fil)
    cat(c("s","deg","N","N","N","C","-","KPa"),
        sep = "\t", file = fil, append = TRUE)
    writeLines("", fil)
    utils::write.table(x, file = fil, append = TRUE,
                       row.names = FALSE, col.names = FALSE, sep = "\t")
    close.connection(fil)
    cat("Wrote ", normalizePath(file), "\n")
}

if (FALSE) {

    lRunNames = getTireRunList()
    lRunNamesSplit = fSplitData(lRunNames, P = 6.894*c(8,10,12,14))

    for (i in seq(1, nrow(lRunNames2))) {
        dfData = readTTCData(lRunNames2$runName[i])
        plotRunConditions(dfData, lRunNames2$runName[i] )
    }


    readData = utils::read.table(
        file = lRunNames[2,2],
        sep = "\t",
        skip = 1,
        header = TRUE,
        as.is = TRUE,
        stringsAsFactors = FALSE)

    SAplot = seq(-12,12,0.05)

    mGridPlot = createFitDataFrame(FYPurePacejka2002, "parameters",
    SA = SAplot,
    IA = c(0,4),
    FZ0 = -1600,
    FZ = c(-250, -500, -750, -1000, -1500),
    parameters = dfFitSummary[2, 9:26])

    plotly::plot_ly(
        mGridPlot, x = ~SA, y = ~FYPurePacejka2002,
        type = 'scatter', mode = 'markers',
        color = ~FZ)

    myplot = plotly::plot_ly()
    myPlot = plotly::add_markers(myPlot, data = mGridPlot, x = ~SA, y = ~FYPurePacejka2002,
                         name = dfFitSummary[2, 1], marker = list(color = "red"))
    show(myPlot)

}

updatelFittingSetup = function(){
    load("./inst/extdata/FYPure2002.Rdata")
    FYPure2002$sfPreProcess = "FYPre"
    save(FYPure2002, file = "./data/FYPure2002.Rdata")

    load("./inst/extdata/FXPure2002.Rdata")
    FXPure2002$sfPreProcess = "FXPre"
    save(FXPure2002, file = "./data/FXPure2002.Rdata")

}

test123 = function() {
clearTirefittingrOptions()

set_test_options()
setFYMF52()

path1 = "C:/Users/benne/Documents/R/tirefittingr/dataignore/B1320run5.dat"
options(tirefittingr.sSavePlotPath = "C:/Users/benne/Documents/R/")

fitTires(path1)
}

}
