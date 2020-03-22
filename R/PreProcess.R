

#' Checks a list of tire runs for further processing
#'
#' Checks a list of tire runs which can be run with the
#'   \code{\link{fitTires}} function.
#'
#' @param svInputFile string vector. A vector of complete file paths of
#'   raw data files. Default value of NA causes a file selection dialog box
#'   to open, where the user can select files dynamically.
#' @param sDefaultPath string. Default file path for the file dialog box.
#'
#' @return string vector. File paths of runs.
#'
#' @examples
#'   \dontrun{
#'   lRunNames = checkTireRunList() #prompts user for files
#'   }
checkTireRunList <- function(svInputFile = NULL, sDefaultPath = getwd()) {

    sAcceptableDataTypes <- c("dat","raw","csv", "txt")
    k = 1
    checkTireRunList = character()

    if (is.null(svInputFile)) {
        #print("Waiting for user to select file in new window. The window may hide behind rStudio, and may not appear in the taskbar.")
        svInputFile = choose_files(default = paste(sDefaultPath,"/data/*",sep = ""),caption = "Select .csv/.raw/.dat file(s)")
        if (length(svInputFile) == 0) { stop("No File Selected. Exiting Function.")}

        message("File Selected.")
    }

    sDirectoryName = dirname(svInputFile[1])
    sRunFolder = paste(dirname(svInputFile[1]),"/",sep = "")

    for (i in seq(1:length(svInputFile))) {
        #not a batch
        if (is.null(svInputFile[i])) {
            warning("filepath in ", i, " index is NULL. Removing it.")
            next
        } else if (is.na(svInputFile[i])) {
            warning("filepath in ", i, " index is NA Removing it.")
            next
        }

        if (exists(svInputFile[i])) {
            #string is of an object that exists in memory
            if(typeof(get(svInputFile[i])) != "list" ){
                warning(svInputFile[i], " exists, but is not a dataframe.",
                        "Removing it.")
                next
            }
        } else {
            #string is file paths
            if (!(tools::file_ext(svInputFile[i]) %in% sAcceptableDataTypes)) {
                warning("File ",basename(svInputFile[i]),
                     " was not a recommended data type. ",
                     "The following extensions are recommended: ",
                     toString(sAcceptableDataTypes))
            }
            if (tools::file_ext(svInputFile[i]) == "m") {
                stop("Matlab .m files are not accepted. ",
                    "Please use .csv, .txt or .dat")
            }
            if (!file.exists(svInputFile[i])) {
                warning("File ",
                     normalizePath(svInputFile[i], mustWork = FALSE),
                     " does not exist. Removing from list.")
                next
            }
        }
        checkTireRunList[k] = svInputFile[i]
        k = k + 1
    }
    return(checkTireRunList)
}


#' Reads Tire Data Files
#'
#' Opens a Tire Data file, optionally dropping rows that do not contain data
#'   such as title row and units row.
#'
#' @param sRunFileName string. Complete path of the file name of a raw
#'   data file, or the name of a dataframe variable already loaded into memory
#'   Note: forward slashes "/"
#'   must be used in R in file paths, not backslashes "\".
#' @param iColNamesRow integer. The row number of the row that contains
#'   the column names. These column names will be the names of the output
#'   data frame.
#' @param iFirstDataRow integer. The row number of the first row that contains
#'   data.
#' @param sep string. The delimiting character used in the text file, such as
#'   "," for a csv file. Use "\\t" for a tab delimited file.
#'   See \link[utils]{read.table} for more details.
#' @return data frame with named columns containing raw data.
#' @export
#' @family Reading Tire Data Functions
#'
#' @examples data = readTTCData("C:/users/data.dat")
readTireData = function(sRunFileName, iColNamesRow = 1,
                             iFirstDataRow = 2, sep = "\t"){

    #check if the file name is actually a variable name loaded into memory
    if (exists(sRunFileName)) {
        readData = get(sRunFileName)
    } else {
        if (!file.exists(sRunFileName)) {
            print(paste("Error: File Not Found ",
                        normalizePath(sRunFileName, mustWork = FALSE)))
            return(NULL)
        }

        readData = utils::read.table(
            file = sRunFileName,
            sep = sep,
            skip = iColNamesRow - 1,
            header = TRUE,
            as.is = TRUE,
            stringsAsFactors = FALSE)

        if (is.na(readData[1,1])) {
          warning(paste("First row contains NA in dataset.",
                        sRunFileName))
        }
        #remove rows between column names and first data
        iDropRows = iFirstDataRow - iColNamesRow - 1
        if (iDropRows > 0) {
            readData <- readData[-seq(1,iDropRows),]
        }
        if(sum(is.na(readData)) > 0) {
            warning("After Reading and processing ", basename(sRunFileName),
                    ", file contains ", sum(is.na(readData)),
                    " NAs")
        }
    }

    readData <- as.data.frame(data.matrix(readData))    #because there was a row of text (the units row) when the data was read, all of the numbers are stored as character type, not double. This converts to double

    return(readData)
}

#' Reads TTC .dat Data Files
#'
#' A Wrapper for \code{\link{readTireData}}
#'   Runs \code{\link{readTireData}} with iColNamesRow = 2 and iFirstDataRow = 4.
#'   `return(readTireData(sRunFileName, iColNamesRow = 2, iFirstDataRow = 4))`
#'
#' @param sRunFileName string. Complete path of the file name of a raw
#'   data file, or the name of a dataframe variable already loaded into memory
#'   Note: forward slashes "/"
#'   must be used in R in file paths, not backslashes "\".
#' @return data frame with named columns containing raw data.
#' @export
#' @family Reading Tire Data Functions
#'
#' @examples
#'   \dontrun{
#'   readTTCData("C:/folder/full_path_to_file.dat") #use forward slashes.
#'   }
readTTCData = function(sRunFileName) {
    return(readTireData(sRunFileName, iColNamesRow = 2, iFirstDataRow = 4))
}

#' Reads TTC .dat Data Files
#'
#' A Wrapper for \code{\link{readTireData}}
#'   Runs \code{\link{readTireData}} with iColNamesRow = 1, iFirstDataRow = 2,
#'   and sep = ",".
#'   `return(readTireData(sRunFileName, iColNamesRow = 1, iFirstDataRow = 2, sep =","))`
#'
#' @param sRunFileName string. Complete path of the file name of a raw
#'   data file, or the name of a dataframe variable already loaded into memory
#'   Note: forward slashes "/"
#'   must be used in R in file paths, not backslashes "\".
#' @return data frame with named columns containing raw data.
#' @export
#' @family Reading Tire Data Functions
#'
#' @examples
#'   \dontrun{
#'   readCSVTireData("C:/folder/full_path_to_file.csv") #use forward slashes.
#'   }
readCSVTireData = function(sRunFileName){
    return(readTireData(sRunFileName,
            iColNamesRow = 1, iFirstDataRow = 2, sep = ","))
}

#' @importFrom rlang .data
removeSASweeps <- function(dfRawData){

    iStartNRow = nrow(dfRawData)
    if (iStartNRow == 0) {stop("Empty data frame.")}

    dfRawData <- dplyr::filter(dfRawData, abs(.data$SA) < 1 )

    return(dfRawData)
}

#' Changes the Options to Run FYPure2002
#'
#' Sets the following options:
#' ```
#'   options(tirefittingr.sdfStartPop = "dfStartParFY",
#'     tirefittingr.sfFittingFunction = "FYPurePacejka2002",
#'     tirefittingr.sfPreProcess = "FYPre",
#'     tirefittingr.sfPlot = "fFYPlot")
#' ```
#'   So that `fitTires()` can be run for FYPure2002 For all available
#'   options, see the helpfile for the \code{\link{fitTires}} function.
#'
#' @export
#'
#' @examples setFYPure2002()
#' @md
setFYPure2002 = function() {
    options(tirefittingr.sdfStartPop = "dfStartParFY",
            tirefittingr.sfFittingFunction = "FYPurePacejka2002",
            tirefittingr.sfPreProcess = "FYPre",
            tirefittingr.sfPlot = "fFYPlot")
}

#' @rdname setFYPure2002
#' @export
setFYMF52 = setFYPure2002

#' Changes Options to Run FXPure.wIA
#'
#' Sets the following options:
#' ```
#'   options(tirefittingr.sdfStartPop = "dfStartParFX",
#'     tirefittingr.sfFittingFunction = "FXPurePacejka2002.wIA",
#'     tirefittingr.sfPreProcess = "FXPre",
#'     tirefittingr.sfPlot = "fFXPlot")
#' ```
#'   So that `fitTires()` can be run for FXPure2002.wIA. For all
#'   available options, see the helpfile for the \code{\link{fitTires}}
#'   function.
#'
#' @export
#'
#' @examples setFXPure2002.wIA()
#' @md
setFXPure2002.wIA = function() {
    options(tirefittingr.sdfStartPop = "dfStartParFX",
            tirefittingr.sfFittingFunction = "FXPurePacejka2002.wIA",
            tirefittingr.sfPreProcess = "FXPre",
            tirefittingr.sfPlot = "fFXPlot")
}

#' @rdname setFXPure2002.wIA
#' @export
setFXMF52 = setFXPure2002.wIA

#' Changes Options to Run FXPure.NoIA
#'
#' Sets the following options:
#' ```
#'   options(tirefittingr.sdfStartPop = "dfStartParFX",
#'     tirefittingr.sfFittingFunction = "FXPurePacejka2002.NoIA",
#'     tirefittingr.sfPreProcess = "FXPre",
#'     tirefittingr.sfPlot = "fFXPlot.NoIA")
#' ```
#'   So that fitTires function can be run for FXPure2002.NoIA. For all
#'   available options, see the helpfile for the \code{\link{fitTires}}
#'   function.
#'
#' @export
#'
#' @examples setFXPure2002.NoIA()
#' @md
setFXPure2002.NoIA = function() {
    options(tirefittingr.sdfStartPop = "dfStartParFX",
            tirefittingr.sfFittingFunction = "FXPurePacejka2002.NoIA",
            tirefittingr.sfPreProcess = "FXPre",
            tirefittingr.sfPlot = "fFXPlot.NoIA")
}

#' Preprocess Data For FY
#'
#' Filters out cold data depending on the setting for
#' options("tirefittingr.coldCutoffTemp"
#' @param dfData data frame. Data to be processed
#'
#' @return data frame. Processed data.
FYPre = function(dfData){

    iStartNRow = nrow(dfData)
    dfData = filterColdData(dfData)
    if ((iStartNRow > 400) && (nrow(dfData) < 400)) {
        if (getOption("tirefittingr.coldCutoffTemp", -Inf) != -Inf) {
        warning("Under 400 data points were left after filtering out all ",
                "cold data containing a temperature below ",
                getOption("tirefittingr.coldCutoffTemp", -Inf),
                ". You may want to turn off cold temp filtering by setting ",
                "options(tirefittingr.coldCutoffTemp = -Inf)")
        }
    }
    if("SA" %in% colnames(dfData)) {
        if (stats::sd(dfData$SA) < 3.5) {
            warning("The standard deviation of SA was low in a dataset for ",
                    "FY analysis. Ensure a FX dataset was not accidentally",
                    "used for FY analysis. Was the correct setFYPure2002(), ",
                    "setFXPure2002.wIA(), or setFXPure2002.wIA() used?")
        }
    }

  #reduces the size of the dataset to speed up optimization
  # set.seed(2017) #sets random number generator, so that every time the code is
  # run, the same samples will be selected from the data
  dfData <- dplyr::sample_n(
    dfData,
    min(getOption("tirefittingr.iDataPoints", default = 4000),
        nrow(dfData)),
    replace = FALSE)
  # set.seed(Sys.time()) #resets the random number generator to be actually
  #random again, so that the optimizer will have a different path every time
  #the code is run

  # dfData = dplyr::rename(dfData, vOutputActual = FY)
  lFitFuncOtherArgs <- list(FZ0 = -1600)
  dfData = cbind(dfData,lFitFuncOtherArgs)

  return(dfData)
}

#' Preprocess Data For FX
#'
#' Depending on the options for
#' @param dfData data frame. Data to be processed
#'
#' @return data frame. Processed data.
FXPre = function(dfData){

    iStartNRow = nrow(dfData)
    dfData = filterColdData(dfData)
    if ((iStartNRow > 400) && (nrow(dfData) < 400)) {
        warning("Under 400 data points were left after filtering out all ",
                "cold data containing a temperature below ",
                getOption("tirefittingr.coldCutoffTemp", -Inf),
                ". You may want to turn off cold temp filtering by setting ",
                "options(tirefittingr.coldCutoffTemp = -Inf)")
    }

    if (getOption("tirefittingr.bFilterSAFromLongitudinal", default = TRUE)) {
        iStartNRow = nrow(dfData)
        dfData = removeSASweeps(dfData)
        if ((iStartNRow > 400) && (nrow(dfData) < 400)) {
            warning("Under 400 data points were left after filtering out all ",
                    "data containing a slip angle outside -1 < SA < 1. You may ",
                    "want to turn off SA filtering by setting ",
                    "options(FilterSAFromLongitudinal = FALSE)")
        }
    }

    #reduces the size of the dataset to speed up optimization
    # set.seed(2017) #sets random number generator, so that every time the code is
    # run, the same samples will be selected from the data
    dfData <- dplyr::sample_n(
        dfData,
        min(getOption("tirefittingr.iDataPoints", default = 4000),
            nrow(dfData)),
        replace = FALSE)
    # set.seed(Sys.time()) #resets the random number generator to be actually
    #random again, so that the optimizer will have a different path every time
    #the code is run

    # dfData = dplyr::rename(dfData, vOutputActual = FX)
    lFitFuncOtherArgs = list(FZ0 = -1600)
    dfData = cbind(dfData,lFitFuncOtherArgs)

    return(dfData)
}

#' Basic Pre-Process for Tire Data
#'
#' Randomly reduces the number of data points to
#'   `getOption("tirefittingr.iDataPoints")` if a value is set.
#'   Defaults to 4000 otherwise. To set, run
#'   `options("tirefittingr.iDataPoints" = 4000")`, where 4000 is the number of
#'   data points you wish to use. For more info on options in tirefitingr, see
#'   the help file for the fitTires function with `?fitTires`.
#'   Works for both lateral and longitudinal.
#'
#' @param dfData data frame. A dataset of tire data to be processed.
#'
#' @return data frame. The tire data trimmed down to the number of datapoints
#'   specified by option("tirefittingr.iDataPoints").
#' @export
#'
#' @examples \dontrun{
#' PreData = basicPre(ABCrun1Lat) #Wnere ABCrun1Lat is a data frame
#' PreData = basicPre(readTTCData("C:/folder/Full_path_name.dat"))
#' }
basicPre = function(dfData) {
    dfData <- dplyr::sample_n(
        dfData,
        min(getOption("tirefittingr.iDataPoints", default = 4000),
            nrow(dfData)),
        replace = FALSE)
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


#' Filters Cold Data from a Dataset
#'
#' @param dfData data frame containing a temperature column named "TSTC".
#'   Defaults to the setting in `getOption("tirefittingr.coldCutoffTemp"`,
#'   which in turn defaults to -Inf, if it is not set. For more info on options in tirefitingr, see
#'   the help file for the fitTires function with `?fitTires`.
#' @param dCutoffTempC numeric. Remove all data before the dataset reaches
#'   this temperature.
#'
#' @return data frame with rows removed before dCutoffTempC is reached.
#' @export
#' @md
filterColdData = function(
    dfData,
    dCutoffTempC = getOption("tirefittingr.coldCutoffTemp", default = -Inf)) {
# browser()
  # #filter out warmup data. Leave only data points after Temp hits 40C
  if ("TSTC" %in% colnames(dfData)) {
    if(all(is.na(dfData$TSTC))) {return(dfData)}
    dfData = stats::na.omit(dfData, cols = c("TSTC"))

    iTempReached <- Position(function(x) {x > dCutoffTempC},dfData$TSTC)
    if (!(is.na(iTempReached))) {
      dfData <- dfData[-1:-iTempReached,]
    }
  }
  return(dfData)
}
