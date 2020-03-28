
choose_directory = function(
    default = getwd(), caption = 'Select data directory') {

    cat("Waiting for user to select a folder. Selection window may",
        "not appear on taskbar and may hide behind RStudio or other windows. ",
        "\n ")
    if (exists('utils::choose.dir')) {
        utils::choose.dir(default, caption = caption)
    } else {
        tcltk::tk_choose.dir(default, caption = caption)
    }
}


choose_files = function(
    default = getwd(), caption = 'Select data directory') {

    cat("Waiting for user to select files. Selection window may",
        "not appear on taskbar and may hide behind RStudio or other windows. ",
        "\n ")
    if (exists('utils::choose.files')) {
        utils::choose.files(default, caption = caption)
    } else {
        tcltk::tk_choose.files(default, caption = caption)
    }
}

#' Calculates RSS Residuals Error Between a Pacejka Curve and Data Set
#'
#' Calculates RSS divided by the total number of data points in the full
#'   data set. See \url{https://en.wikipedia.org/wiki/Residual_sum_of_squares}
#'   {https://en.wikipedia.org/wiki/Residual_sum_of_squares}.
#' @param svInputFile string vector. Path names or names of data frames
#' @param parameters dataframe or numeric vector. Pacejka parameters.
#'
#' @return data frame. First column is RSS divided by number of datapoints.
#'   Second column is number of data points.
#' @export
calculateRSS = function(svInputFile = NULL, parameters) {
    if (is.null(svInputFile)) {
        #print("Waiting for user to select file in new window. The window may hide behind rStudio, and may not appear in the taskbar.")
        svInputFile = choose_files(
            default = paste(getwd()),
            caption = "Select .csv/.raw/.dat file(s)")
        if (length(svInputFile) == 0) {
            stop("No File Selected. Exiting Function.")
        }
        message("File Selected.")
    }
    #read, formats, and validates data
    fReadTire = getOption("tirefittingr.sfReadTireFile", "readTTCData")
    if (!exists(fReadTire)) {
        stop("Function ", fReadTire, " does not exist. Set the option with ",
             "options(tirefittingr.sfReadTireFile = 'YOUR_FUNCTION_NAME'),",
             " or set it back to the default of readTTCData with ",
             "options(tirefittingr.sfReadTireFile) = NULL")
    }
    if(!checkRequiredOption("tirefittingr.sfFittingFunction", TRUE)) {
        return(0)
    }
    fFittingFunction = get(getOption('tirefittingr.sfFittingFunction'))
    fReadTire = get(fReadTire)
    checkFittingFunctionAttr(fFittingFunction)

    RSS = data.frame(residual = numeric(), nDataPoints = numeric())

    # if (is.list(svInputFile)) {
    #     #if a dataframe was directly passed
    #     dfData = svInputFile
    #     endLoop = 1
    # } else if (typeof(svInputFile) == "character") {
        endLoop = length(svInputFile)
    # } else {
    #     stop("Invalid data type")
    # }

    for (i in 1:endLoop) {

        if (exists(svInputFile[i], where = parent.frame())) {
            dfData = get(svInputFile[i], envir = parent.frame())
                    #Raw data file was directly passed by name
        } else {
            dfData = fReadTire(svInputFile[i])
        }
        RSS[i,2] = nrow(dfData)

        dfData = fDropColsThatArentArgs(dfData, fFittingFunction)
        dfData = dplyr::rename(dfData,
                        "vOutputActual" = attr(fFittingFunction, "outputName"))

        if (is.list(parameters)) {
            vParameters = unlist(parameters[i,])
        } else {
            vParameters = parameters
        }

        args = c(list(
            vParameters = vParameters,
            fFitFunction = fFittingFunction),
            as.list(c(dfData)))

        RSSError = do.call(
            what = fOptimizeFunctionWrapper,
            args = args
        ) / RSS[i,2]

        RSS$residual[i] = RSSError
    }
    return(RSS)
}

normalizePathwEndSlash = function(path, mustExist = FALSE) {

    right = function(x,n){
        substring(x,nchar(x)-n+1)
    }
    path = normalizePath(path, winslash = "/", mustWork = FALSE)
    path = paste(path) # converts \\ to /
    if (right(path,1) != "/") {
        path = paste(path,"/", sep = "")
    }
    pathwSlash = path
    pathNoSlash = substring(pathwSlash,1, nchar(pathwSlash)-1)

    if (!(file.exists(pathwSlash) || (file.exists(pathNoSlash)))){
        if (mustExist) {
            stop("Folder doesn't exist: ", pathwSlash)
        } else {
            warning("Folder doesn't exist: ", pathwSlash)
        }
    }
    return(path)
}

checkFittingFunctionAttr = function(fFittingFunction) {

    if (is.null(attr(fFittingFunction, "parameterNames"))) {
        stop("A attribute named 'parameterNames'is missing from the fitting",
             " function. The fitting function is your pacejka or equivalent",
             "function")
    }
    if (is.null(attr(fFittingFunction, "outputName"))) {
        stop("A attribute 'outputName' is missing from the fitting function",
             "The fitting function is your pacejka or equivalent function")
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
                    "fitting function, preprocess function, and plotting",
                    "function set. Run setFYPure2002() before fitTires to",
                    "avoid this message in the future. See ?setFYPure2002().\n")
        } else if (usrInput %in% c("x","X")){
            setFXPure2002.wIA()
            cat("\nFunction setFXPure2002.wIA() was run. Starting population,",
                    "fitting function, preprocess function, and plotting",
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

fDropColsThatArentArgs = function(
    dfData,
    fFittingFunction,
    sColKeep = attr(fFittingFunction, "outputName")) {

    svDataColName = colnames(dfData)
    svFunctionArgs =  names(formals(fFittingFunction))

    svRequiredArgs = names(formals(
        fFittingFunction)[!nzchar(formals(fFittingFunction))] )

    svNamesReqDiff = dplyr::setdiff(svRequiredArgs,svDataColName) #vPars

    if (length(svNamesReqDiff) != 1) {
        warning(
            "Error matching fitting function arguments with dataset ",
            "columns. All of the arguments without defaults of the fitting ",
            "function (except the one argument containing the fitting ",
            "parameters) must exactly match column names in the dataset. ",
            "Unmatched arguments: ",
            toString(svNamesReqDiff), " Dataset Columns: ",
            paste(svDataColName), " A common cause is setting the options for ",
            "FX, but using FY data. Ensure the correct setFYPure2002() or ",
            "setFXPure2002.wIA() was run first." )}

    svColsToKeep = dplyr::intersect(svFunctionArgs,svDataColName) #vPars
    if (!is.na(sColKeep)) svColsToKeep = c(svColsToKeep, sColKeep)
    return(dfData[,svColsToKeep])
}

#' Write Table to File With Header
#'
#' Writes a table to a text file using the header from an existing file. This
#'   makes it easy to re-write data files into the same format as they were
#'   received.
#'
#' @param x object to be written. Usually a data frame or matrix.
#' @param file either a character string naming a file or a connection open for
#'   writing. "" indicates output to the console. See \link[utils]{write.table}.
#' @param sHeaderFile string. Filename of a file to copy the header from
#' @param nRowHeader integer. Default is 3. Number of rows in the header in the
#'   header file. This many rows will be copied from the header file and
#'   inserted at the top of the new file.
#' @param sep string. Default is "\\t" Values on each line of the file are
#'   separated by this character. see \link[utils]{write.table}.
#' @param verbose boolean. Default is FALSE. TRUE prints more messages in
#'   console.
#' @seealso \link[utils]{write.table} \link[utils]{read.table}
#' @export
#'
writeFileWithHeader = function(x, file, sHeaderFile, nRowHeader = 3,
                               sep = "\t", verbose = FALSE) {

    if(!(file.exists(sHeaderFile))) {
        stop(paste0("sHeaderFile Does not exists:", normalizePath(sHeaderFile)))
    }
    dfHeaders = utils::read.table(
        file = sHeaderFile,
        sep = sep, header = FALSE, as.is = TRUE, fill = TRUE,
        nrows = nRowHeader, stringsAsFactors = FALSE)

    iColDiff = ncol(x) - ncol(dfHeaders)
    if (iColDiff > 0) {

        iBestMatch = sum(colnames(x) %in% dfHeaders[1,])
        iBestRow = 1
        # print(paste0("Best Match:", iBestMatch, " BestRow:", iBestRow))

        if (nRowHeader > 1) {
            for (i in 2:nRowHeader) {
                iMatches = sum(colnames(x) %in% dfHeaders[i,])
                if (iMatches > iBestMatch) {
                    #New match must beat the best match, so it there is a tie,
                    # the first row is used
                    iBestMatch = iMatches
                    iBestRow = i
                }
                # print(paste0("Best Match:", iBestMatch, " BestRow:", iBestRow))
            }
        }
        # dfHeaders = dplyr::bind_cols(dfHeaders, x, )
        for (i in ncol(dfHeaders):(ncol(dfHeaders) + iColDiff)) {
            dfHeaders[iBestRow,i] = colnames(x)[i]
        }
    }
    if(ncol(dfHeaders) != ncol(x)) {
        stop("writing error. ncol(dfHeader) not equal ncols(df) 'x'.")
    }
    utils::write.table(x = dfHeaders, file = file,
                       row.names = FALSE, sep = sep, col.names = FALSE,
                       quote = FALSE)
    utils::write.table(x = x, file = file,
                       append = TRUE , row.names = FALSE, sep = sep,
                       col.names = FALSE, quote = FALSE)
    if (verbose) {
        cat("writing:", file, "\n - with ",
            nrow(x),"rows of data\n")
    }
}
