
choose_directory = function(default, caption = 'Select data directory') {
    cat("Waiting for user to select a folder. Selection window may",
        "not appear on taskbar and may hide behind RStudio or other windows. ",
        "\n ")
    if (exists('utils::choose.dir')) {
        utils::choose.dir(default, caption = caption)
    } else {
        tcltk::tk_choose.dir(default, caption = caption)
    }
}


choose_files = function(default, caption = 'Select data directory') {
    cat("Waiting for user to select files. Selection window may",
        "not appear on taskbar and may hide behind RStudio or other windows. ",
        "\n ")
    if (exists('utils::choose.files')) {
        utils::choose.files(default, caption = caption)
    } else {
        tcltk::tk_choose.files(default, caption = caption)
    }
}

checkRequiredOption = function(sOptionName,
                               bMustExist = FALSE){
    if (is.null(getOption(sOptionName))) {
        if (getOption('tirefittingr.testMode', FALSE)) {
            usrInput = (getOption('tirefittingr.testInput', ""))
        } else {
            cat("\nThe required option ", sOptionName, " was not set.",
                "To set all default settings for MF5.2 or Pacejka2002,\n",
                "Enter 'X' for LONGITUDINAL \n",
                "Enter 'Y' for LATERAL \n",
                "Enter anything else to quit.\n")
            usrInput = readline()
        }
        if (usrInput %in% c("y","Y")){
            setFYPure2002()
            cat("\nFunction setFYPure2002() was run. Starting population,",
                    "fitting function, pre-process function, and plotting",
                    "function set. Run setFYPure2002() before fitTires to",
                    "avoid this message in the future. See ?setFYPure2002().\n")
        } else if (usrInput %in% c("x","X")){
            setFXPure2002.wIA()
            cat("\nFunction setFXPure2002.wIA() was run. Starting population,",
                    "fitting function, pre-process function, and plotting",
                    "function set. Run setFXPure2002.wIA() before fitTires to",
                    "avoid this message in the future. See ",
                    "?setFXPure2002.wIA(). \n")
        } else {
            cat("Quitting.")
            return(FALSE)
        }

    }
    if (bMustExist && !exists(getOption(sOptionName))) {
        stop("Error: ", sOptionName, "does not point to an object that exists ",
             "in memory. No object exists named ",
             getOption(sOptionName))
    }
    return(TRUE)
}



set_test_options = function(){
    #speed up solver.
    options(
        tirefittingr.iParallelCores = 1, #or assign NA to detect how many cores
        tirefittingr.iEvolIterMax = 15,     #300, 800 #maximum iterations of the solver
        tirefittingr.iDataPoints = 1000,   #4000  #using all the datapoints from the raw datafile would take forever to solve, so instead this many randomly selected points are used                                          # Data points to show in the plot only
        tirefittingr.sSavePlotPath = FALSE, #True saves plots as png, False displays graphs in RStudio
        tirefittingr.bPlotRunConditions = TRUE,
        tirefittingr.sSummaryExportPath = NA,
        tirefittingr.verbose = FALSE)

    options(tirefittingr.testMode = TRUE)
}

#' Clears all options from tirefittingr
#'
#' @export
#'
#' @examples clearTirefittingrOptions()
clearTirefittingrOptions = function() {
    options(tirefittingr.coldCutoffTemp = NULL,
            tirefittingr.iDataPoints = NULL,
            tirefittingr.iEvolIterMax = NULL,
            tirefittingr.bFilterSAFromLongitudinal = NULL,
            tirefittingr.sfFittingFunction = NULL,
            tirefittingr.iParallelCores = NULL,
            tirefittingr.sfPlot = NULL,
            tirefittingr.bPlotRunConditions = NULL,
            tirefittingr.sfPreProcess = NULL,
            tirefittingr.sfReadTireFile = NULL,
            tirefittingr.sSavePlotPath = NULL,
            tirefittingr.sdfStartPop = NULL,
            tirefittingr.verbose = NULL)
    #Testing ones
    options(tirefittingr.testInput = NULL)
    options(tirefittingr.testMode = NULL)
    options(tirefittingr.testEndEarly = NULL)
    options(tirefittingr.bDebugMode = NULL)
}
