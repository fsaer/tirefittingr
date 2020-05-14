

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
#'   Runs \code{\link{readTireData}} with `iColNamesRow = 2` and `iFirstDataRow` = 4.
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
#'   Runs \code{\link{readTireData}} with `iColNamesRow = 1`,
#'   `iFirstDataRow = 2`, and `sep = ","`.
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

#' Changes the Options to Run `FYPure2002`
#'
#' Sets the following options:
#' ```
#'   options(tirefittingr.sdfStartPop = "dfStartParFY",
#'     tirefittingr.sfFittingFunction = "FYPurePacejka2002",
#'     tirefittingr.sfPreProcess = "FYPre",
#'     tirefittingr.sfPlot = "fFYPlot")
#' ```
#'   So that `fitTires()` can be run for `FYPure2002` For all available
#'   options, see the help file for the \code{\link{fitTires}} function.
#'   Also see \code{\link{FYPurePacejka2002}}
#'
#' @export
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
#'   So that `fitTires()` can be run for `FXPure2002.wIA`. For all
#'   available options, see the help file for the \code{\link{fitTires}}
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

#' Changes Options to Run `FXPure.NoIA`
#'
#' Sets the following options:
#' ```
#'   options(tirefittingr.sdfStartPop = "dfStartParFX",
#'     tirefittingr.sfFittingFunction = "FXPurePacejka2002.NoIA",
#'     tirefittingr.sfPreProcess = "FXPre",
#'     tirefittingr.sfPlot = "fFXPlot.NoIA")
#' ```
#'   So that `fitTires` function can be run for `FXPure2002.NoIA`. For all
#'   available options, see the help file for the \code{\link{fitTires}}
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
#' `options("tirefittingr.coldCutoffTemp")`
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

#' Basic Preprocess for Tire Data
#'
#' Randomly reduces the number of data points to
#'   `getOption("tirefittingr.iDataPoints")` if a value is set.
#'   Defaults to 4000 otherwise. To set, run
#'   `options("tirefittingr.iDataPoints" = 4000")`, where 4000 is the number of
#'   data points you wish to use. For more info on options in tirefitingr, see
#'   the help file for the `fitTires` function with `?fitTires`.
#'   Works for both lateral and longitudinal.
#'
#' @param dfData data frame. A dataset of tire data to be processed.
#'
#' @return data frame. The tire data trimmed down to the number of datapoints
#'   specified by `option("tirefittingr.iDataPoints")`.
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

#' Filters Cold Data from a Dataset
#'
#' @param dfData data frame containing a temperature column named `TSTC`.
#'   Defaults to the setting in `getOption("tirefittingr.coldCutoffTemp"`,
#'   which in turn defaults to `-Inf`, if it is not set. For more info on options in tirefitingr, see
#'   the help file for the `fitTires` function with `?fitTires`.
#' @param dCutoffTempC numeric. Remove all data before the dataset reaches
#'   this temperature.
#'
#' @return data frame with rows removed before `dCutoffTempC` is reached.
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

#' Split Tire Data Files
#'
#' Splits Tire data files into multiple data files based on a passed parameter
#'   and saves the new files.
#'
#' Opens each data file from svRunNames, then splits all of the
#'   data into the closest match in the splitting parameter argument.
#'   For example if the fitting parameter was `P = c(50,60,80)`, data points with
#'   pressures from 0-45 kPa would be dropped. 45-54 kPa would be put in the
#'   50 kPa, 55-69 kPa would be in the 60kPa file, 70-90 kPa would be in
#'   the 80 kPa file, and data points with pressures of 91+ kPa would be
#'   dropped. If, after filtering, a file would contain less than 1% of the size
#'   of the original data, then that file is skipped instead of writing a csv
#'   or recording it in the output dataframe. Note that even though units of
#'   'psi' will appear in text output and file names, metric units (kPa) must
#'   be passed into the function, and kPa will be in the output files.
#'
#'   The tire data is read using the function named in the settings
#'   `getOption(tirefittingr.sfReadTireFile)`. If the option is not set (`NULL`),
#'   then the default function `readTTCData` is used, which works for .dat
#'   files from the TTC. For more on this option, see \code{\link{fitTires}}.
#'
#' @param svRunPaths optional string vector. Defaults to `NULL.`
#'   Complete file path of a tire raw data file. NULL opens a file dialog box
#'   for the user to select files.
#' @param svRunNames optional string vector. Defaults to `NULL.` String vector the
#'   same length as svRunPaths. Run names for your future reference.
#'   Used as titles for plots. Also gets recorded in summary table. Default `NULL`
#'   uses the end of the file name.
#' @param ... named argument of a Numeric Vector containing at least two values,
#'   with name corresponding to a column name of a tire data file.
#'   Suggested Names: 'V', 'N', 'SA', 'IA', 'P', 'FZ', 'TSTC', 'SR'.
#' @param  verbose boolean. Defaults to `TRUE`.
#'   True prints messages in the console, while false does not.
#' @param bSaveBatchNames boolean. Defaults to `FALSE`. `TRUE` saves output
#'   to a csv file.
#' @param k integer. Defaults to 10.
#'   Rolling average window width. A rolling
#'   average is applied to the named argument column before splitting.
#'   set to 1 to disable. See `k` from
#'   \code{\link[zoo:rollmean]{zoo::rollmean()}}
#' @param nRowHeader see `rRowHeader` from \code{\link{writeFileWithHeader}}
#' @return Returns Dataframe of full-path run names of newly created runs.
#'   Second column contains copied `svRunNames`.
#' @export
#'
#' @examples
#'   \dontrun{
#'   #split into files containing pressures 8, 10, 12, and 14 psi
#'   #with a .dat TTC data file
#'   lRunNamesSplit = splitTireDataAndSave( P = c(55.2,68.9,82.7,96.5))
#'
#'   #open a csv file, where the first column has complete file paths
#'   #Note the file selection window often hides behind RStudio!
#'   svFiles = read.csv(choose.files(), stringsAsFactors = FALSE)[,1]
#'   if (!file.exists(svFiles[1])) {stop("File Doesn't Exist")} #check the file path is correct!
#'
#'   lRunNamesSplit = splitTireDataAndSave( P = c(55.2,68.9,82.7,96.5))
#'   lRunNamesSplit = splitTireDataAndSave( P = 6.894*c(8,10,12,14))
#'   }
#'
#' @importFrom rlang .data
#' @importFrom zoo rollmean
splitTireDataAndSave = function(svRunPaths = NULL,
                                svRunNames = NULL,
                                ...,
                                verbose = TRUE,
                                bSaveBatchNames = FALSE,
                                k = 10,
                                nRowHeader = 3) {

  svRunPaths = checkTireRunList(svRunPaths) #NULL prompts user to select files
  # dfRunNamesFullPath = NULL,

  #    P = 6.89476*c(8.0, 10.0, 12.0, 14.0)
  lSplitBy = list(...)
  if (length(lSplitBy) > 1) {
    stop("fSplitData Can only handle ONE variable to split by. Variables: ",
         paste(as.list(names(lSplitBy)), collapse =", "))
  } else if (length(lSplitBy) == 0) {
    stop("fSplitData requires second argument- a value to split the data by.
             For example 'P = c(68.9,82.7)")
  }
  dvSplitVal = lSplitBy[[1]]
  sVarName = names(lSplitBy[1])
  if (is.null(sVarName)) {
    stop("The passed argument to split by must have a name that matches
             a column name in the dataset. For example 'P = c(68.9,82.7)")
  } else if (length(dvSplitVal) == 1) {
    stop("Spliting data requires at least two buckets to split into.
             So that the code can find out how wide to make each bucket.")
  }

  dDiff = base::diff(dvSplitVal)

  dvLowCutoff = dvSplitVal - c(0.5*dDiff[1], 0.5*dDiff)
  dvHighCutoff = dvSplitVal + c(0.5*dDiff, 0.5*dDiff[length(dDiff)])

  dfRunNamesOutput = data.frame(FileName = character(),
                                RunName = character(),
                                stringsAsFactors = FALSE)

  sfTireFunction =
    getOption("tirefittingr.sfReadTireFile", "readTTCData")
  if (!exists(sfTireFunction)) {
    stop(fTireFunction, " function does not exist. Reset to the default of",
         " readTTCData with options(tirefittingr.sfReadTireFile = NULL)")
  }
  for (i in seq(from = 1, to = length(svRunPaths))) {
    dSumRows = 0
    sPathNameFull = svRunPaths[i]
    fTireFunction = get(sfTireFunction)
    dfDataFull = fTireFunction(sPathNameFull) #read, formats, and validates data
    dNRowDfData = nrow(dfDataFull)

    if (!(sVarName %in% base::colnames(dfDataFull))) {
      stop("The passed variable name '", sVarName, "' could not be
                     found in the column names of the dataset: ",
           paste(base::colnames(dfDataFull), collapse = ", "))
    }

    # dfHeaders =    utils::read.table(
    #     file = sPathNameFull,
    #     sep = "\t", header = FALSE, as.is = TRUE, fill = TRUE,
    #     nrows = 3, stringsAsFactors = FALSE)

    dfDataFull$VarRollAvg = zoo::rollmean(
      dfDataFull[,sVarName],
      k, align = "center", fill = c("extend", NA, NA))

    for (j in seq(from = 1, to = length(dvSplitVal))) {
      dfDataPart = dplyr::filter(
        dfDataFull, .data$VarRollAvg < dvHighCutoff[j])
      dfDataPart = dplyr::filter(
        dfDataPart, .data$VarRollAvg > dvLowCutoff[j])

      if (sVarName == "P") {
        VarOutput = paste(round(dvSplitVal[j]/6.89476, 1),"psi",
                          sep = "")
        sSuffix = paste0("-", VarOutput)
      } else {
        VarOutput = round(dvSplitVal[j], 1)
        sSuffix = paste0("-", VarOutput, "-",sVarName)
      }

      if (nrow(dfDataPart) > (0.01*dNRowDfData)) {
        #write a new data file
        dSumRows = dSumRows + nrow(dfDataPart)

        sWriteSplitDataPath = paste0(
          tools::file_path_sans_ext(sPathNameFull),
          sSuffix,
          ".", tools::file_ext(sPathNameFull))
        dfDataPart = dplyr::select(dfDataPart, -"VarRollAvg")


        writeFileWithHeader(
          dfDataPart,
          file = sWriteSplitDataPath,
          sHeaderFile = sPathNameFull,
          nRowHeader = nRowHeader,
          verbose = verbose)

        if (is.null(svRunNames)) {
          svRunNames = basename(sWriteSplitDataPath)
        }

        dfRunNamesOutput = plyr::rbind.fill(
          dfRunNamesOutput,
          data.frame(
            FileName = sWriteSplitDataPath,
            RunName = paste0(svRunNames[i], sSuffix)))
      } else {
        #file doesn't have enough data points in this pressure range to
        #be worth creating a file
        if (verbose) {
          cat("Not enough rows ( <", round(0.01*dNRowDfData),
              ") of Data to write ", sVarName, " = ",VarOutput, "\n",
              "(Original file didn't have enough data points",
              "matching this splitby criteria)\n")
        }
        next
      }
    }
    dataKeptPercent = round(100 * dSumRows / dNRowDfData, 1)
    if (verbose) {
      cat("Split Complete.\n")
      cat(dataKeptPercent,
          "% of original data file was written into the new split file(s)",
          "\n")
    }
    if (dataKeptPercent < 25) {
      warning(cat("Only ", dataKeptPercent,
                  "% of original data file was written into the new split file(s)"
      ))
    }

  }

  sBatchNameExportPath = paste(
    dirname(svRunPaths[i]),"/SplitOutputBatchNames.csv",
    sep = "")


  dfRunNamesOutput[] = lapply(dfRunNamesOutput, as.character) #convert factor

  if (bSaveBatchNames) {
    dfBatchNamesExport = dfRunNamesOutput
    dfBatchNamesExport[,2] = basename(dfBatchNamesExport[,2])
    utils::write.table(
      dfBatchNamesExport,
      file = sBatchNameExportPath,
      row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
    if (verbose) {
      cat("BatchNames file written to: ", sBatchNameExportPath, "\n")
    }
  }
  return(dfRunNamesOutput)
}

