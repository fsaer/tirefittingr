
#Function to optimize

fOptimizeFunctionWrapper = function(
    vParameters, fFitFunction, vOutputActual, ...) {

    #dfData must ONLY contain columns that exactly match arguments of fFitFunction
    #This function gets called many many times, so its speed is important

    vFuncPredict <- fFitFunction(..., parameters = vParameters)

    vDeviation <- vFuncPredict - vOutputActual   #error between predicted value and raw data value

    # dLeastSquaresDeviation <- sqrt(sum(vDeviation*vDeviation))/sum()
    dRSSError <- sqrt(sum(vDeviation*vDeviation))
    return(dRSSError)
}

fTireDEoptimWrapper = function(
  dfDataProcessed,
  fFittingFunction,
  dfStartPop,
  clMyCluster = NULL) {

  if (is.null(attr(fFittingFunction, "parameterNames"))) {
      stop("A attribute named 'parameterNames'is missing from the fitting",
           " function. The fitting function is your pacejka or equivalent",
           "function")
  }
  if (is.null(attr(fFittingFunction, "outputName"))) {
    stop("A attribute 'outputName' is missing from the fitting function",
         "The fitting function is your pacejka or equivalent function")
  }

  if (!all(attr(fFittingFunction, "parameterNames") %in%
           colnames(dfStartPop))) {
      stop("All Function parameter names, \n",
               paste(attr(fFittingFunction, "parameterNames"), collapse = ", "),
               "\n must be in the starting population columns: \n",
               paste(colnames(dfStartPop), collapse = ", "), "\n")
  }


  myControl <- DEoptim::DEoptim.control(
    itermax = getOption("tirefittingr.iEvolIterMax", 300),
    trace = getOption("tirefittingr.trace", FALSE),
    strategy = 2,
    initialpop = as.matrix(dfStartPop),
    NP = NA,
    CR = 0.9,     #I did a few tests and varied CR & F to find which combo had the best results (CR=0.9, F = 0.7). Then I increased the population and iterations and let the code run for a week.
    F = 0.7,
    cluster = clMyCluster
  )
# browser()
  dfStartPop = stats::na.omit(dfStartPop)
  if (nrow(dfStartPop) < 10 ) {
      stop("Fewer than the minimum 10 rows were found in dfStartPop. 10x number
           of fitting parameters is recommended")
  }
  #get upper and lower limits from the min and max of the starting population
  lower = apply(dfStartPop, 2, min, na.rm = TRUE)
  upper = apply(dfStartPop, 2, max, na.rm = TRUE)

  dfDataProcessed = fDropColsThatArentArgs(dfDataProcessed, fFittingFunction)
  dfDataProcessed = dplyr::rename(dfDataProcessed,
      "vOutputActual" = attr(fFittingFunction, "outputName"))

  #format arguments
  args = c(list(
    fn = fOptimizeFunctionWrapper,
    control = myControl,
    lower = lower,
    upper = upper,
    fFitFunction = fFittingFunction),
    as.list(c(dfDataProcessed)))

  #call the big bad optimizer
  fTireDEoptimWrapper = do.call(
    what = DEoptim::DEoptim,
    args = args)
  return(fTireDEoptimWrapper)
}

appendBadRun <- function(sMessage, fModelSummary, svRunPaths, svRunNames, i) {

    fModelSummary <- plyr::rbind.fill(
        fModelSummary,
        data.frame(
            RunName = svRunNames,
            FileName = svRunPaths,
            Iterations = 0,
            Status = sMessage,
            stringsAsFactors = FALSE))
    print(c(i, as.character(fModelSummary$Name[i]), fModelSummary$Status[i]))
    return(fModelSummary)
}

#' Main Tire Fitting Function
#'
#' Creates a summary of coefficients and meta data of each data file passed
#'   in the through the svRunPaths argument.
#'
#' Opens each raw data file using \code{\link{readTireData}} (or a similar
#'   function if the option tirefittingr.sfReadTireFile is defined), then applies
#'   the pre-process function defined in getOption("tirefittingr.sfPreProcess").
#'   Using \code{\link[DEoptim:DEoptim]{DEoptim::DEoptim()}}, a differential evolution
#'   optimization algorithm, the starting population of
#'   parameters evolves to find those
#'   that best represent the data. In post-processing, a comparison of the
#'   fitted curve to the data is plotted.
#'
#' To use presets, run `setFYPure2002()`, `setFYMF52()`, `setFXPure2002.wIA()`,
#'   `setFXPure2002.NoIA()`, or `setFXMF52()` before running `fitTires()`.
#'   See `?setFYPure2002` etc for more info.
#' Manually set with `options("tirefittingr.exampleOption" = value)`. Use
#'   `?options` for more info.
#'   To find out what an option is currently set as use
#'   `getOption("tirefittingr.exampleOption")`.
#'   \describe{
#'     \item{tirefittingr.coldCutoffTemp}{numeric. defaults to -Inf Used for
#'       removing the warmup from the start of a dataset. All data collected
#'       before the tires reach this temperature will be ignored.}
#'     \item{tirefittingr.iDataPoints}{integer. defaults to 4000. Number of
#'       data points to use. Data will randomly sampled down to this many
#'       data point to speed up solve time.}
#'     \item{tirefittingr.iEvolIterMax}{integer. defaults to 300. Maximum
#'       number of iterations of the solver.}
#'     \item{tirefittingr.bFilterSAFromLongitudinal}{logical. defaults to TRUE.
#'       When pre-processing data for an FX fit, should the data be filtered
#'       so that only datapoints with -1 < SA < 1 be left?}
#'     \item{tirefittingr.sfFittingFunction}{string. The name of the fitting
#'       function to use. Pre-installed options are `'FYPurePacejka2002'`,
#'       `'FYPureMF52'`, `'FXPureMF52'`, `'FXPurePacejka2002.NoIA'`, and
#'       `'FXPurePacejka2002.wIA'`.}
#'     \item{tirefittingr.iParallelCores}{integer. defaults to 1. Number of
#'       cores to use. Assign NA to detect how many cores are available}
#'     \item{tirefittingr.sfPlot}{string of a function name.
#'       A plotting function to run after fitting to check quality of fit.
#'       Pre-installed options are `'fFYPlot'`, `'fFXPlot'`.}
#'     \item{tirefittingr.bPlotRunConditions}{logical. defaults to TRUE. TRUE
#'       also plots the run conditions.}
#'     \item{tirefittingr.sfPreProcess}{string of a function name.
#'       A pre-processing function to apply to the data before fitting.
#'       Pre-installed options are `'FYPre'`, `'FXPre'`.}
#'     \item{tirefittingr.sfReadTireFile}{string of a function name. Defaults
#'       to 'readTTCData'. Pre-installed options are `'readTTCData'`` and
#'       `'readCSVTireData'`. Make your own by writing a
#'       wrapper function for \code{\link{readTireData}}.
#'       The function must have one argument that is
#'       the full path name of a raw data file, and must output a data frame.
#'       See \code{\link{readTTCData}}.}
#'     \item{tirefittingr.sSavePlotPath}{string. defaults to FALSE. Entire path
#'       of location to save the plot. FALSE displays them in the rStudio
#'       graphics window.}
#'     \item{tirefittingr.sdfStartPop}{string of the name of a
#'       data frame. A data frame of the starting population of pacejka
#'       coefficients. Pre-installed options are `'dfStartParFY'`,
#'       and `'dfStartParFX'`.}
#'     \item{tirefittingr.verbose}{logical. Defaults to TRUE. False suppresses
#'       most messages.}
#'   }
#'
#' @param svRunPaths optional string vector. Defaults to NULL.
#'   Complete file path of a tire raw data file. NULL opens a file dialog box
#'   for the user to select files.
#' @param svRunNames optional string vector. Defaults to NULL. String vector the
#'   same length as svRunPaths. Run names for your future reference.
#'   Used as titles for plots. Also gets recorded in summary table. Default NULL
#'   uses the end of the file name.
#' @param sSummaryExportFolder string. Defaults to NULL, which
#'   won't save anything. Folder to export the results summary of all of the fit
#'   data. Suggested to use `getwd()` to save to the working directory.
#'
#' @return data frame. Summary of each raw data file that was fit along with
#'   parameters in alphabetical order.
#' @export
#'
#' @examples
#' options("tirefittingr.iParallelCores" = 2)
#' setFYPure2002() # setFXPure2002.wIA()
#' \dontrun{
#' dfFitSummary = fitTires()
#' }
#' @md
#' @importFrom rlang .data
fitTires <- function(svRunPaths = NULL, svRunNames = NULL, sSummaryExportFolder = NULL) {

    svRunPaths = checkTireRunList(svRunPaths) #NULL prompts user to select files
    if (is.null(svRunNames)) svRunNames = basename(svRunPaths)

    # initialize model summary
    modelSummary = data.frame(
        RunName = character(),
        FileName = character(),
        Iterations = integer(),
        Status = character(),
        Tavg = numeric(), Tsd = numeric(), RSS = numeric(),
        numberOfPoints = integer())

    # sSummaryExportFolder = getOption("sSummaryExportFolder", NULL)
    if (!is.null(sSummaryExportFolder)) {
        if (is.na(sSummaryExportFolder)) {
            sSummaryExportFolder = choose_directory(getwd())
        }
        sSummaryExportPath = paste(
            sSummaryExportFolder,"_",
            gsub(Sys.time(), pattern = ":", replacement = ""),
            "-Summary",
            sep = "")
    } else sSummaryExportPath = NULL

    bverb = getOption('tirefittingr.verbose', TRUE)
    # initialize parallel
    iParallelCores = getOption("tirefittingr.iParallelCores", default = 1)
    if (iParallelCores <= 0) {
      iParallelCores = (max(1,parallel::detectCores() - 1))
    }
    if (iParallelCores > 1) {
        clMyCluster = parallel::makeCluster(iParallelCores)
    } else clMyCluster = NULL

    # Check settings and set up functions
    if(!checkRequiredOption("tirefittingr.sdfStartPop", TRUE)) return(0)
    if(!checkRequiredOption("tirefittingr.sfFittingFunction", TRUE)) return(0)

    fFittingFunction = get(getOption('tirefittingr.sfFittingFunction'))
    dfStartPop = get(getOption('tirefittingr.sdfStartPop'))

    if (is.null(attr(fFittingFunction, "parameterNames"))) {
        stop("A attribute named 'parameterNames'is missing from the fitting",
             " function. The fitting function is your pacejka or equivalent",
             "function")
    }
    if (ncol(dfStartPop) == 0) {
        stop("The starting parameter matrix has 0 columns")
    }
    #drop rows that aren't in the parameter names
    dfStartPop = dfStartPop[, attr(fFittingFunction, "parameterNames")]
    dfStartPop = dfStartPop[, order(names(dfStartPop))]

    if (ncol(dfStartPop) == 0) {
        stop("The starting population matrix has 0 columns left after filtering",
             " out only columns that matched the names of the fitting function",
             "parameters: ", paste(
                 attr(fFittingFunction, "parameterNames"),
                 collapse = ", "))}
    #read, formats, and validates data
    fReadTire = getOption("tirefittingr.sfReadTireFile", "readTTCData")
    if (!exists(fReadTire)) {
        stop("Function ", fReadTire, " does not exist. Set the option with ",
             "options(tirefittingr.sfReadTireFile = 'YOUR_FUNCTION_NAME'),",
             " or set it back to the default of readTTCData with ",
             "options(tirefittingr.sfReadTireFile) = NULL")
    }
    fReadTire = get(fReadTire)

    # Timing
    dStartTime <- proc.time()  #store the current time to measure how long this takes to run
    dTimeEst <- getOption("tirefittingr.iEvolIterMax", 300) *
        getOption("tirefittingr.iDataPoints", 4000) *  170 / 300 / 4000 / 180 /
        iParallelCores
    if (getOption("tirefittingr.bDebugMode", FALSE)) {
      print("Debug mode: ON, if you don't want debug mode on,
            set bDebug to FALSE")
      }
    if (bverb) cat("\n Starting loop through all selected runs.")
    # browser()
    if (!(is.null(dTimeEst)) && !(length(dTimeEst) == 0)) {
        if (bverb) { cat(" Estimated Run Time: ")
          cat(round(0.75*dTimeEst, digits = 2)," to ",
            round(1.1*dTimeEst, digits = 2) ,"min/run or ",
            round(0.75*dTimeEst*length(svRunPaths), digits = 1),
            " to ",
            round(1.1*dTimeEst*length(svRunPaths), digits = 1),
            " minutes total. \n")
        }
    }
    if (bverb) {
        cat("\n")
        cat("", "Plot Status:","Run Name:","","Fitting Error:","","\n" ,
            sep = "\t")
    }
    for (i in seq(from = 1, to = length(svRunPaths))) {

      dfDataFull <- fReadTire(svRunPaths[i])

      #validate if data set is missing
      suppressWarnings(
        if (is.null(dfDataFull[1])) {
          modelSummary <- appendBadRun(
            paste("File Not Found: ",svRunPaths[i]),
              modelSummary,svRunPaths[i], svRunNames[i],i)
          next
        }
      )
      if (!is.null(getOption('tirefittingr.sfPreProcess'))) {
        fPreProcess = get(getOption('tirefittingr.sfPreProcess'))
        dfDataProcessed = fPreProcess(dfDataFull)
      } else {
          warning("No Pre-processing function found!",
                  "Set a pre-processing function with options(tirefittingr.",
                  "sfPreProcess = 'FYPre') or options(tirefittingr.",
                  "sfPreProcess = 'FXPre') or options(tirefittingr.",
                  "sfPreProcess = 'basicPre'). These will reduce the number",
                  " of data points used in fitting to speed up runtime. ",
                  "Run ?FYPre ?FXPre or ?basicPre in console to see help file.")
          dfDataProcessed = dfDataFull
      }
      if (getOption("tirefittingr.testEndEarly", FALSE)) return("Test Complete")

      lastRun = tryCatch(
        {
          fitModel = fTireDEoptimWrapper(
            dfDataProcessed,
            fFittingFunction,
            dfStartPop,
            clMyCluster = clMyCluster)

          #Post Process
          lEvolParm <- as.list(fitModel$optim$bestmem)
          names(lEvolParm) <- names(dfStartPop)

          lastRun <- data.frame(
            RunName = svRunNames[i],
            FileName = basename(svRunPaths[i]),
            Iterations = fitModel$optim$iter,
            Status = "Fitted",
            Tavg = ifelse("TSTC" %in% colnames(dfDataProcessed),
              mean(dfDataProcessed$TSTC),
              NA),
            Tsd = ifelse("TSTC" %in% colnames(dfDataProcessed),
              stats::sd(dfDataProcessed$TSTC),
              NA),
            RSS = fitModel$optim$bestval,
            numberOfPoints = nrow(dfDataProcessed),
            as.data.frame(lEvolParm),
            stringsAsFactors = FALSE)
        },
        error = function(cond) {

        warning("Caught Error: ", cond)
        lastRun = data.frame(
            RunName = svRunNames[i],
            FileName = svRunPaths[i],
            Iterations = 0,
            Status = cond[[1]],
            stringsAsFactors = FALSE)
          return(lastRun)
        }
      )
      #add the last run to the summary
      modelSummary <- plyr::rbind.fill(modelSummary,as.data.frame(lastRun))
      modelSummary = modelSummary[ , order(names(modelSummary))]
      modelSummary = dplyr::select(modelSummary,
          .data$RunName, .data$FileName,
          .data$Iterations, .data$Status,
          .data$Tavg, .data$Tsd,
          .data$RSS, .data$numberOfPoints,
          dplyr::everything())

      #saves a copy of the model summary every 5 runs.
      if ( all(
            getOption("tirefittingr.backupSummary", FALSE),
            (i %% 5) == 0,
            !is.null(sSummaryExportPath)
            )) {

          utils::write.csv(
            x = modelSummary,
            file = paste(sSummaryExportPath,"-",i,"-.csv",sep = ""),
            row.names = FALSE)
      }

      if (getOption("tirefittingr.bSavePlot", FALSE )) {
          sSavePath = paste(svRunPaths[i],"-Row",i,".png",sep = "")
      } else (sSavePath = NULL)

      if (modelSummary[i,"Status"] != "Fitted") {
        sPlotStatus = "Not plotted- Model did not fit."
        warning(sPlotStatus)
      } else if (is.null(getOption('tirefittingr.sfPlot'))) {
          sPlotStatus = paste0("option tirefittingr.sfPlot not defined. ",
                "No Plot function. see ?tireFittingr.")
      } else if (!exists(getOption('tirefittingr.sfPlot'))) {
          sPlotStatus = paste("Could not find plotting function ",
                              getOption('tirefittingr.sfPlot'))
      } else {
          fPlot = get(getOption('tirefittingr.sfPlot'))
          sPlotStatus = fPlot(dfDataProcessed, modelSummary[i,])
      }

      #concatenate and print status of the run
      if (bverb) {cat(i, sPlotStatus,as.character(modelSummary$Name[i]),"",
          modelSummary$RSS[i],"","\n" ,sep = "\t")
      }

      next
    } #close for loop

    if (bverb) cat("Complete. \n")
    if (!is.null(sSummaryExportPath)) {
        utils::write.csv(
            x = modelSummary,
            file = paste(
                sSummaryExportPath, "-Complete.csv"),
            row.names = FALSE)

        print(paste("Parameters Output to: ",
                    sSummaryExportPath,
                    "-Complete.csv", sep = ""))
    }
    if (bverb) { print(
        c(" Runtime in minutes:",
          round((proc.time()["elapsed"] - dStartTime["elapsed"]) / 60.0,
                digits = 2)))
    }
    if ( iParallelCores != 1) {parallel::stopCluster(clMyCluster)}
    #showConnections()
    #closeAllConnections() # causes issues with knitr
    return(modelSummary)
}


