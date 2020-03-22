


#' Compare Fitted Curve to Raw Data
#'
#' Plots raw data with a fitted curve overlayed so that the user can judge the
#'   quality of the fit.
#'
#' UNTESTED OUTSIDE OF fitTires function!
#'
#' @param dfPlotData data frame. Contains raw data to plot.
#' @param lCurveParameters list containing the parameters for the curves.
#'   Any values containing NA will be silently dropped.
#' @param lPlotSetup list containing the following named items: \describe{
#'   \item{fFitFunction}{string. Contains the name of a magic formula such as
#'   FYPurePacejka2002.}
#'   \item{iDataPointsPlot}{integer. Number of raw data points to plot.}
#'   \item{sInCol}{Name of the column of raw data that will be plotted on the
#'   x axis.}
#'   \item{sOutCol}{Name of the column of raw data that will be plotted on the
#'   y axis.}
#'   \item{sSweep}{Name of one column of raw data, as well as an argument in
#'   the magic formula that will be swept through a range of values.}
#'   \item{lConst}{Named list of inputs to the magic formulas that will be
#'   kept constant. Such as list(IA = 0)}
#'   \item{xlab}{x axis label}
#'   \item{ylab}{y axis label}}
#'
plotGoodnessOfFit = function(dfPlotData, lCurveParameters, lPlotSetup){

    # plotGoodnessOfFit(dfDataFull, sPlotTitle, lCurveParameters, lPlotSetup, sSavePath )
    #Note: x and y in this section represent dependent and independent
    #variables on a graph. nothing to do with forward or sideways directions

    lCurveParameters <- base::Filter(Negate(anyNA), lCurveParameters)

    # dfPlotData = na.omit(dfPlotData)
    # dfDataProcessed = fDropColsThatArentArgs(dfDataProcessed, fFittingFunction, sColKeep = NA)

    sPlotTitle = ifelse(
        !is.null(lPlotSetup$sPlotTitle),
        lPlotSetup$sPlotTitle,
        "Untitled")
    sSavePath = lPlotSetup$sSavePath
    bPlotRunConditions = lPlotSetup$bPlotRunConditions

    # tryCatch({
    sInCol = lPlotSetup$sInCol
    sOutCol = lPlotSetup$sOutCol

    #initialization
    # if (is.null(sPlotID)){sPlotID = gsub(Sys.time(), pattern = ":", replacement = "")}
    # sPNGPath<-paste(sRunFolder,sRunName,"-",sPlotID,".png",sep = "")  #concatenate
    # sPNGPath <- sSavePath

    #create empty data table to be populated
    dfx = data.table::data.table(sInCol = NA,"IA" = NA,"FZ" = NA)
    xmin = stats::quantile(dfPlotData[,sInCol], c(0.01))  #lPlotSetup$xmin
    xmax = stats::quantile(dfPlotData[,sInCol], c(0.99)) #lPlotSetup$xmax
    ymin = stats::quantile(dfPlotData[,sOutCol], c(0.01))#lPlotSetup$ymin
    ymax = stats::quantile(dfPlotData[,sOutCol], c(0.99)) #lPlotSetup$xmax#lPlotSetup$ymax
    myStep = (xmax - xmin) / 80
    xlab = lPlotSetup$xlab
    ylab = lPlotSetup$ylab

    dfPlotDataCopy <- dfPlotData #make copy
    dfPlotData = fDropColsThatArentArgs(dfPlotData, lPlotSetup$fFitFunction)
    # sParArgName = getParametersArgName() #attr(lPlotSetup$fFitFunction, "parameterNames")

    x <- seq(from = xmin, to = xmax, by = myStep) # Create starting x values to plot curve. These will be overwritten later

    # dfPlotData <- dfPlotData[(dfPlotData[,sInCol] > (xmin)) & (dfPlotData[,sInCol] < (xmax)),] #filter out data points from the raw data that wont be on the chart

    data.table::setnames(dfx,"sInCol",sInCol) #change the name of the column named "sInCol" to be SA or SL

    iAppendLength = 0

    #plotting
    #starts the png file
    if (!is.null(lPlotSetup$sSavePath)) {
        grDevices::graphics.off();
        grDevices::png(filename = lPlotSetup$sSavePath,
                       width = 1000, height = 1000)
    }

    #create empty plot (wont show until plot is saved or displayed)
    graphics::plot(NULL,
         xlim = c(xmin,xmax),
         ylim = c(ymin,ymax),
         main = sPlotTitle,  # Plot title
         xlab = xlab,
         ylab = ylab
    )

    #The FZ from the raw data is supposed to be in categories such as 100lb, 150lb, 200lb etc.
    #However, it varies significantly from this intended load, especially at high slip angles.
    #When plotting a curve of best fit, it is difficult to visually see the quality of the fit
    #if the curve is plotting Fy values for Fz=1556N, while most of the raw data is actually at 1700N.
    #Solution: Separate the data into buckets based on their SA/SL (ex. 0< SA <2), then find the 5 most common FZ values
    # within this bucket, and plot your curve at this FZ.

    #Create iNumBigSteps number of buckets for the SA or SL.
    iNumBigSteps <- 16 #number of buckets
    myBigStep <- (max(x) - min(x)) / iNumBigSteps #size of bucket
    dfPlotData[,sInCol] <- round(dfPlotData[,sInCol] / myBigStep) * myBigStep  #rounds SA/SL to the center SA/SL of each bucket

    dvUniqueInputVals = unique(dfPlotData[,sInCol])

    for (q in seq(1, length(dvUniqueInputVals))) {
        subsetA = (abs(dfPlotData[,sInCol] - dvUniqueInputVals[q]) < myStep)
        if (sum(subsetA) < 15 ) {
            message(paste("Not enough data points to plot x-axis value of :", dvUniqueInputVals[q]))
            next
        }

        x <- seq(from = dvUniqueInputVals[q] - myBigStep/2,
                 to = dvUniqueInputVals[q] + myBigStep/2,
                 by = myStep) #create x within this peak and bucket to plot the curve

        if (stats::sd(dfPlotData[subsetA,lPlotSetup$sSweep]) > 10) {
            hHistSweep <- graphics::hist(
                as.numeric(dfPlotData[subsetA,lPlotSetup$sSweep]),
                plot = FALSE,
                breaks = seq(from = min(dfPlotData[subsetA,lPlotSetup$sSweep]),
                             to = max(dfPlotData[subsetA,lPlotSetup$sSweep]),
                             by = (max(dfPlotData[subsetA,lPlotSetup$sSweep]) -
                                       min(dfPlotData[subsetA,lPlotSetup$sSweep])) / 45))

            mySweep <- IDPmisc::peaks(hHistSweep$mids,hHistSweep$counts)#, minPH = 20) #, minPW=20) #detects peaks of the histogram. minPh and minPW can adjust the minimum height and width for something to be counted as a peak

            suppressWarnings(

                if (length(mySweep$y) == 1) {
                    #prevents an error in the event there's only one peak
                    mySweep = mySweep$y
                } else if (is.na(mySweep$y)) {
                    mySweep = 0
                } else {
                    #reorder the peaks... I think its by height
                    mySweep <- mySweep[ order((mySweep$x), decreasing = TRUE), 1]
                    mySweep = removeSimilarValues(mySweep, dWithin = 150)
                    mySweep = mySweep[1:5] #select only the top 5 peaks
                }
            )
        } else mySweep = mean(dfPlotData[subsetA,lPlotSetup$sSweep])
        # loop through the sweep
        for (r in seq(1, length(mySweep))) {
            subsetB = abs(dfPlotData[, lPlotSetup$sSweep] - mySweep[r]) < 25

            dfTempVars <- eval(substitute(data.frame(var = x), list(var = sInCol)))
            names(dfTempVars) <- sInCol

            dfTemp2 <- as.data.frame(lPlotSetup$lConst)
            dfTemp3 <- data.frame(sSweepTemp = mySweep[r])
            names(dfTemp3) <- lPlotSetup$sSweep
            lTempVars <- as.list(cbind(dfTempVars,dfTemp2, dfTemp3))
            lTempVars[["parameters"]] <- as.numeric(lCurveParameters)

            y <- do.call(lPlotSetup$fFitFunction, args = lTempVars)

            graphics::points(dfPlotDataCopy[subsetA & subsetB, sInCol],
                   dfPlotDataCopy[subsetA & subsetB, sOutCol],
                   pch = 19, cex = 0.4,
                   col = RColorBrewer::brewer.pal(5,"Pastel2")[r])#rgb(0, max(0, min(255, -outcome[k,j]/10)),0,alpha=255,maxColorValue=255)) #points puts the raw data points onto the graph
            graphics::lines(x,
                  y,
                  pch = 19, cex = 0.6,
                  col = RColorBrewer::brewer.pal(5,"Set2")[r],
                  lwd = 3)#rgb(0, max(0, min(255, -outcome[k,j]/10)),0,alpha=255,maxColorValue=255)) #points puts the raw data points onto the graph
        }
    }

    if (!is.null(lPlotSetup$sSavePath)) {
        grDevices::dev.off()
        grDevices::graphics.off()
        # if cat("Plot saved to \n", lPlotSetup$sSavePath)
    } #saves and ends the png started earlier. Graphics.off sets the graphic device back to rstudio
    return("Plot Succeeded")
}

#' Creates a data frame with a column
#'
#' @param fFittingFunction function. A tire fititng function. Pre-installed
#'   functions available are FYPurePacejka2002, FXPurePacejka2002.NoIA, and
#'   FXPurePacejka2002.wIA . The function must have assigned attributes
#'   "parameterNames", which list the names of the parameters.
#'   Example: attr(fFittingFunction, "parameterNames") =
#'   c("pC1", "pD1", "pD2" ...)
#' @param sParameterArgName string. Defaults to "parameters".
#'   Name of the parameters argument in the
#'   curve fitting function. For built in curves such as FYPurePacejka2002,
#'   the parameter name is 'parameters'.
#' @param ... Other arguments passed into the curve fitting function. Pass
#'   vectors in order to sweep through multiple values. The exception being
#'   the item containing the vector of parameters, which  will not be
#'   swept through.
#'
#' @return data frame with a column for each original item from ... and the
#'   output of the fFittingFunction. This data frame is ready for plotting.
#' @export
#'
#' @examples data = createFitDataFrame(
#'   FYPurePacejka2002,
#'   sParameterArgName = "parameters",
#'   SA = seq(-12,12,0.25),
#'    IA = 0,
#'   FZ = c(-250, -500, -750, -1000, -1500),
#'   parameters = c(20,5.89,-1.23,-12.23,1.87,-0.75,0.08,90.44,0,0.02,
#'   -0.18,187.77,11.81,-23.34,-0.04,-0.14,0.42,0.02))
createFitDataFrame = function(fFittingFunction,
                              sParameterArgName = "parameters", ...) {
    # lInput = list(SA = seq(-12,12,0.25),
    #              IA = c(0,4),
    #              FZ0 = -1600,
    #              FZ = c(-250, -500, -750, -1000, -1500),
    #              parameters = dfFitSummary[1, 8:25])
    # sParameterArgName = "parameters"
    lInput = list(...)
    lInputNoPar = lInput
    lInputNoPar[sParameterArgName] = NULL

    if (!sParameterArgName %in% names(lInput)) {
        stop("The sParameterArgName must appear as the name of a
... argument. ", sParameterArgName, " did not appear in ",
             paste(names(lInput), collapse = ", "))
    }

    svRequiredArgs = names(formals(
        fFittingFunction)[!nzchar(formals(fFittingFunction))] )

    if (!all(svRequiredArgs %in% names(lInput))) {
        stop("Must pass named arguments in ... for all of the required",
             " arguments of the fFittingFunction. ",
             "\n Names Arguments ", paste(names(lInput), collapse = ", "),
             "\n fFittingFunction Required Arguments:",
             paste(svRequiredArgs, collapse = ", "),"\n")
    }
    if (!all(names(lInput) %in% names(formals(fFittingFunction)))) {
        stop("Additional named arguments in ... do not match the arguments",
             " of the fitting function.\n Unused argument: ",
             paste(base::setdiff(names(lInput), collapse = ", ")),
             paste(names(formals(fFittingFunction)), collapse = ", "),
             "\n")
    }

    dvParameters = stats::na.omit(as.numeric(lInput[[sParameterArgName]]))


    parms = attr(fFittingFunction, "parameterNames")
    if (is.null(parms)) {stop("Function must have assigned attribute ",
        "parameterNames. See ?createFitDataFrame")
    }
    if (length(dvParameters) != length(parms)) {
        stop(paste("The parameter vector passed had a different number of
parameters (after removing NAs) than the pacejka fitting function requires.
Check that the fitting function and passed parameters are for the same
function. Passed Parameters:", length(dvParameters), "vs Function:",
                   length(attr(fFittingFunction, "parameterNames"))))
    }

    mGridNoPar = expand.grid(lInputNoPar)
    x = c( list( dvParameters ), mGridNoPar)
    ListNames = names(x)
    ListNames[1] = sParameterArgName
    names(x) = ListNames

    mGridNoPar[, attr(fFittingFunction, "outputName")] =
        do.call(fFittingFunction, args = x)
    return(mGridNoPar)
}

#' Plotting Function for FY
#'
#' Used by the fitTires function. Set with `setFYPure2002()` or manually with
#'   `options(tirefittingr.sfPlot = "fFYPlot")`.
#'
#' @param dfPlotData data frame. Data to plot.
#' @param runSummary data frame. From the fitTires function.
fFYPlot = function(dfPlotData, runSummary) {
    lPlotSetup = list(
        fFitFunction = FYPurePacejka2002,
        iDataPointsPlot = 12000,
        sInCol = "SA",
        sOutCol = "FY",
        sSweep = "FZ",
        lConst = list(IA = 0, FZ0 = -1600),
        xlab = "SA (deg)",
        ylab = "Fy (N)")

    if (!all(attr(lPlotSetup$fFitFunction, "parameterNames") %in%
             colnames(runSummary))) {
        missingPar = setdiff(
            attr(lPlotSetup$fFitFunction, "parameterNames"),
            colnames(runSummary))
        stop("Plotting parameter(s) ", paste(missingPar, collapse = ", "),
             " were not found in the ",
             "runSummary dataframe column names.")
    }

    svPlotStatus = NULL

    for (i in seq(from = 1, to = nrow(runSummary))) {
        lCurveParameters = as.list(
            runSummary[i,attr(lPlotSetup$fFitFunction, "parameterNames")])

        lPlotSetup$sPlotTitle = runSummary[i,1]
        #   paste(
        # runSummary[i,"Name"], "-row", rownames(runSummary[i,]), sep = "")
        if (is.character(getOption("tirefittingr.sSavePlotPath", FALSE))) {
            lPlotSetup$sSavePath = paste(
                normalizePath(getOption("tirefittingr.sSavePlotPath")), "/",
                runSummary$FileName[i], "-Row", rownames(runSummary[i,]), ".png",
                sep = "")
        } else {lPlotSetup$sSavePath = NULL}

        svPlotStatus[i] = tryCatch({
            svPlotStatus[i] = plotGoodnessOfFit(
                dfPlotData, lCurveParameters, lPlotSetup)
        }, error = function(err) {
            return(paste("Plot Error:", err$message))
        })
        if (getOption("tirefittingr.bPlotRunConditions", default = FALSE)) {
            NoMessage = plotRunConditions(
                dfPlotData, lPlotSetup$sPlotTitle, lPlotSetup$sSavePath)
        }
    }
    return(svPlotStatus)
}

#' Plotting Function for FX
#'
#' Used by the fitTires function. Set with `setFXPure2002.wIA()` or
#'   manually with
#'   `options(tirefittingr.sfPlot = "fFXPlot")`.
#'
#' @param dfPlotData data frame. Data to plot.
#' @param runSummary data frame. From the fitTires function.
#' @param IA logical. Indicates if IA should be plotted or not. Set to true if
#'   the pacejka fitting equation used IA as an input. Set FALSE otherwise.
fFXPlot = function(dfPlotData, runSummary, IA = TRUE) {

    lPlotSetup = list(
        iDataPointsPlot = 12000,
        sInCol = "SL",
        sOutCol = "FX",
        sSweep = "FZ",
        lConst = list(IA = 0, FZ0 = -1600),
        xlab = "SL",
        ylab = "Fx (N)")
    if (IA) {
        lPlotSetup$fFitFunction = FXPurePacejka2002.wIA
        lPlotSetup$lConst = list(IA = 0, FZ0 = -1600)
    } else {
        lPlotSetup$fFitFunction = FXPurePacejka2002.NoIA
        lPlotSetup$lConst = list(FZ0 = -1600)
    }

    if (!all(attr(lPlotSetup$fFitFunction, "parameterNames") %in%
             colnames(runSummary))) {
        missingPar = setdiff(
            attr(lPlotSetup$fFitFunction, "parameterNames"),
            colnames(runSummary))
        stop("Plotting parameter(s) ", paste(missingPar, collapse = ", "),
             " were not found in the ",
             "runSummary dataframe column names.")
    }

    svPlotStatus = NULL
    for (i in seq(from = 1, to = nrow(runSummary))) {
        lCurveParameters = as.list(
        runSummary[i,attr(lPlotSetup$fFitFunction, "parameterNames")])
    lPlotSetup$sPlotTitle = runSummary[i,1]
    # lPlotSetup$sPlotTitle = paste(
    #   runSummary[i,"Name"], "-row", rownames(runSummary[i,]), sep = "")
    if (is.character(getOption("tirefittingr.sSavePlotPath", FALSE))) {
      lPlotSetup$sSavePath = paste(
        getOption("tirefittingr.sSavePlotPath"), "/",
        runSummary$FileName[i], "-Row", rownames(runSummary[i,]), ".png",
        sep = "")
    } else {lPlotSetup$sSavePath = NULL}

    svPlotStatus[i] = tryCatch({
    svPlotStatus[i] = plotGoodnessOfFit(dfPlotData, lCurveParameters, lPlotSetup)
    }, error = function(err) {
        return(paste("Plot Error:", err$message))
    })
    if (getOption("tirefittingr.bPlotRunConditions", default = FALSE)) {
        NoMessage = plotRunConditions(
            dfPlotData, lPlotSetup$sPlotTitle, lPlotSetup$sSavePath)
    }
  }
  return(svPlotStatus)
}

fFXPlot.NoIA = function(dfPlotData, runSummary) {

    return(
        fFXPlot(dfPlotData,runSummary, IA = FALSE)
    )
}

#' Get Parameter Argument Name
#'
#' Returns the name of the parameter argument in the fFittingFunction
#'
#' Returns the name of the parameter argument in the fFittingFunction
#'   by looking for what argument of the fFittingFunction does not appear
#'   as a column name in the dataset.
#'
#' @param dfData data frame containing column names that match to
#'   all of the argument names of the fFittingFunction
#' @param fFittingFunction function. Function whose arguments will be matched
#'   against the column names of dfData to determine the name of the parameter
#'   argument.
#'
#' @return string. Argument name.
getParametersArgName = function(dfData, fFittingFunction){
    svDataColName = colnames(dfData)

    dfData = fDropColsThatArentArgs(dfData, fFittingFunction)
    # svFunctionArgs =  names(formals(fFittingFunction))

    svRequiredArgs = names(formals(
        fFittingFunction)[!nzchar(formals(fFittingFunction))] )

    sParameterArgName = dplyr::setdiff(svRequiredArgs, svDataColName)
    if (length(sParameterArgName) != 1) {stop("Invalid column names matching
                                             fitting function arguments")}
    return(sParameterArgName)
}

#' Removes Duplicates
#'
#' Removes duplicates from a list to post-process peaks from
#'   IDPmisc::peaks function. Removes duplicates by averaging 2 values
#'   that are within the dWithin value.
#'
#' @param x numeric vector
#' @param dWithin numeric. How close will values be to trigger averaging
#'   the two values into one?
#'
#' @return numeric vector where the values close enough to be duplicates
#'   are removed by averaging them together.
removeSimilarValues = function(x, dWithin) {

    if (length(x) < 2) { return(x)}
    dvOutput = NULL; dvOutput[1] = Inf
    j = 1
    bSkipOneLoop = FALSE
    x = x[order(x, decreasing = TRUE)]

    for (i in seq(1, length(x))) {

        if (bSkipOneLoop) {
            bSkipOneLoop = FALSE
            next
        } else if (i == length(x)) {
            dvOutput[j] = x[i]
            return(dvOutput)
        } else if (abs(x[i] - x[i + 1]) < dWithin) {
            dvOutput[j] = mean(c(x[i], x[i + 1]))
            j = j + 1
            bSkipOneLoop = TRUE
        } else {
            dvOutput[j] = x[i]
            j = j + 1
        }
    }
    return(dvOutput)
}

#' Plots columns vs time of tire data
#'
#' @param dfData data frame. Processed tire data.
#' @param sRunName string. Name of the run.
#' @param sSavePath string. Defaults to NULL. Path to save the plot to. NULL
#'   does not save the plot, and instead displays in the plot window.
#'
#' @return string. "Plot Run Conditions Successful". If no errors occurred.
#'
#' @examples
#' \dontrun{
#' setFYPure2002() ## or setFXPure2002()
#' plotRunConditions(ABCrun1LongPreProccessed)
#' }
plotRunConditions = function(dfData, sRunName, sSavePath = NULL) {

    # svColumns = c("ET","TSTC","SR","SA","FZ", "P")

    if (!"ET" %in% base::colnames(dfData)) {
        dfData$ET = as.numeric(rownames(dfData))
    }

    dfData = dfData[ base::order(dfData$ET), ]


    if (!is.null(sSavePath)) {
        NoMessage = plotRunConditionsBase(dfData, sRunName, sSavePath)
    } else {
        NoMessage = plotRunConditionsPlotly(dfData, sRunName, bShow = TRUE)
    }
    return("Plot Run Conditions Successful")
}

plotRunConditionsBase = function(dfData, sRunName, sSavePath) {

    grDevices::graphics.off()
    grDevices::png(filename = paste(
    tools::file_path_sans_ext(sSavePath),
    "-Conditions.png",
    sep = ""),
    width = 1300,
    height = 800)

    dfPlotSetup = data.frame(
        var = character(), yLab = character(), col = character(),
        stringsAsFactors = FALSE)

    dfPlotSetup[1,] = c("IA", "IA (deg) GREY", "grey")
    dfPlotSetup[2,] = c("SR", "SR (-) BLUE", "blue")
    dfPlotSetup[3,] = c("SA", "SA (deg) GREEN", "darkgreen")
    dfPlotSetup[4,] = c("FZ", "FZ (N) BLACK", "black")
    dfPlotSetup[5,] = c("P", "P (kPa) PURPLE", "purple")
    dfPlotSetup[6,] = c("TSTC", "Temp (C) RED", "red3")

    yAxisLabels = c("", "", "", "")
    k = 1

    for (i in seq(1, nrow(dfPlotSetup))) {
        if (dfPlotSetup$var[i] %in% base::colnames(dfData)) {
            graphics::par(mar = c(4.5, 8.1, 3, 8.1))

            graphics::plot(
                dfData$ET,
                dfData[,dfPlotSetup$var[i]],
                axes = FALSE,
                type = 'l',
                col = dfPlotSetup$col[i],
                main = basename(sRunName),
                xlab = "ET", ylab = "")
            graphics::par(new = TRUE)

            graphics::axis(at = pretty(dfData[,dfPlotSetup$var[i]]),
                 side = (1 + (k %% 2)) * 2,
                 line = ceiling(k / 2) * 2 - 2,
                 col = dfPlotSetup$col[i])
            yAxisLabels[(1 + (k %% 2)) * 2] =
                paste(yAxisLabels[(1 + (k %% 2)) * 2],
                      dfPlotSetup$yLab[i], "/")

            graphics::par(new = TRUE)
            k = k + 1
        }
    }
    graphics::par(new = TRUE)
    graphics::axis(at = pretty(dfData$ET), side = 1)
    graphics::par(new = TRUE)
    graphics::mtext(yAxisLabels[2], line = k, side = 2)
    graphics::mtext(yAxisLabels[4], line = k, side = 4)


    grDevices::dev.off()
    grDevices::graphics.off()

}

#' @importFrom rlang .data
#' @importFrom magrittr %>%
plotRunConditionsPlotly = function(
    dfData,
    sRunName,
    varShow = c("KEEP","ET","FX","FY","FZ","SA","SR","IA","P","IA"),
    bShow = TRUE) {

    if (sum(varShow %in% colnames(dfData)) > 1) {
        dfData = dplyr::select(dfData, intersect(varShow, colnames(dfData)))
    }
    if (!("ET" %in% colnames(dfData))) {
        dfData$ET = seq(1,nrow(dfData))
    }

    dataLong = dfData %>%
        tidyr::pivot_longer(-.data$ET)

    dataLong$id = as.numeric(factor(dataLong$name))

    p = plotly::plot_ly(dataLong, x = ~ET, y = ~value, color = ~name, colors = "Dark2",
                yaxis = ~paste0("y", id)) %>%
        plotly::add_markers() %>%
        plotly::subplot(nrows = max(dataLong$id), shareX = TRUE)

    if (bShow) methods::show(p)
    return(p)

}

