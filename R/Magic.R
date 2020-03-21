
#' MF5.2 / Pacejka 2002 Pure Lateral Magic Formula
#'
#' Calculates the lateral tire force given FZ, SA, IA, and a set of parameters
#'   using the Pacejke 2002 pure lateral formula. FYPureMF52 is for
#'   Magic Formula 5.2 Pure lateral, which is the same as the Pacejka 2002 Pure
#'   Lateral Model. FYPureMF52 function is an alias to FYPurePacejka2002.
#'
#' Check out the source code on the tirefittingr github in /R/Magic.R.
#'
#' Works in the SAE Sign conventions: Fz <0, (See Race Car Vehicle Dynamics
#' by Milliken, p39).
#'   or \url{https://www.oreilly.com/library/view/tire-and-vehicle/
#'   9780080970165/images/F150016bm01-9780080970165.jpg}
#'
#' @param FZ numeric. Normal Load in Newtons.
#' @param SA numeric. Slip Angle in degrees.
#' @param IA numeric. Inclination Angle in degrees.
#'   Similar to camber angle, except the positive/negative sign has a
#'   different convention. Caution: While fitting to data, it is highly
#'   recommended that you use a dataset containing a sweep of different IA
#'   values. While you can fit to a dataset that only uses one IA value,
#'   any model created this way will output garbage if any value of
#'   IA is input other than the one it was originally fit at.
#' @param Vs numeric. Tire Slip velocity in kph. Defaults to 40 kph.
#' @param FZ0 numeric. Nominal rated load in Newtons. Defaults to -1600N.
#'   Typically the highest load used in the data.
#'   See section 4.3.2 of Tyre and Vehicle Dynamics (Pacejka, 2002)
#' @param V0 numeric. Reference Velocity in kph. Defaults to 40 kph.
#'   Typically the average velocity of the test.
#' @param parameters numeric Vector. 18 Pacejka Pure-Lateral
#'   coefficients in the following order:
#'   pC1, pD1, pD2, pD3, pE1, pE2, pE3, pE4, pK1, pK2, pK3, pV1, pV2,
#'   pV3, pV4, pH1, pH2, pH3
#' @param scaleMu numeric. Defaults to 1. Scaling factor multiplied by mu.
#'   Used as symbol 'lambda_mu_y' in Tyre and Vehicle Dynamics (Pacejka, 2002)
#'   Set to 1 during fitting. Testing from a facility (eg. TTC) is
#'   done on a sandpaper "road" which has a higher coefficient of friction
#'   (mu) than regular pavement. To use this function to estimate real forces
#'   that would be seen on pavement (for lapsim, etc), mu must be scaled
#'   down to what can be expected from a road. Typical mu on sandpaper may be
#'   2.5, while pavement may be 1.4 (roughly equal to max steady state lateral
#'   Gs without Aerodynamic downforce). As such a scaling factor of
#'   scaleMu = 1.6/2.5 = 0.64 may be used so that the output will be reasonable
#'   for a tire on pavement.
#'
#' @return Lateral Force, FY, in Newtons.
#' @examples  params = c(20,5.89,-1.23,-12.23,1.87,-0.75,0.08,90.44,0,0.02,
#'   -0.18,187.77,11.81,-23.34,-0.04,-0.14,0.42,0.02)
#'   FY = FYPurePacejka2002(
#'       FZ = -1000,
#'       SA = 10,
#'       IA = 0,
#'       parameters = params)
#'   #Often used within createFitDataFrameToPlot function to create a plotable
#'   #data frame.
#'   mGridPlot = createFitDataFrame(FYPurePacejka2002, "parameters",
#'   SA = seq(from = -12, to = 12, by = 0.1), IA = 0,
#'   FZ = c(c(-250, -750, -1200)),
#'   parameters = params)
#' @export
#' @references Tyre and Vehicle Dynamics (Pacejka, 2002)
#' @keywords pacejka
#' @seealso \code{\link{FXPurePacejka2002.NoIA}}
#' @family MagicFormulas
FYPurePacejka2002 = function(FZ, SA, IA, Vs = 40,
                             FZ0 = -1600, V0 = 40, parameters, scaleMu = 1) {
    pC1 = parameters[1]
    pD1 = parameters[2]
    pD2 = parameters[3]
    pD3 = parameters[4]
    pE1 = parameters[5]
    pE2 = parameters[6]
    pE3 = parameters[7]
    pE4 = parameters[8]
    pH1 = parameters[9]
    pH2 = parameters[10]
    pH3 = parameters[11]
    pK1 = parameters[12]
    pK2 = parameters[13]
    pK3 = parameters[14]
    pV1 = parameters[15]
    pV2 = parameters[16]
    pV3 = parameters[17]
    pV4 = parameters[18]

    #Convert to radians.
    alpha = SA * pi / 180
    gammaStar = sin( IA * pi / 180)    #4.E4, and degrees to radians conversion

    dfz = (FZ - FZ0) / FZ0                                     #4.E2
    alpha = alpha + (pH1 + pH2 * dfz) + pH3 * gammaStar         #4.E28 & #4.E20
    Kalpha0 = pK1 * FZ0 * sin(2 * atan(FZ / (pK2 * FZ0)))               #4.E25
    # Note for Kalpha, other sources have used
    # ...* (1 - pK3 * abs(gamma)), no "gammastar^2"
    # May be a 1996 vs 2002 model difference. I haven't seen the 1996 model.
    Kalpha = Kalpha0 * (1 - pK3 * gammaStar^2)                 #4.E26.
    C = pC1                                                              #4.E21
    # 1996 model doesn't include .../ (1 + Vs / V0). It was added in 2002 model.
    mu = (pD1 + pD2 * dfz) * (1 - pD3 * gammaStar^2) * scaleMu /
        (1 + Vs / V0)   #4.E23
    D = mu * FZ                                                          #4.E22
    B = Kalpha / (C * D + 0.001)       #e = 0.001                        #4.E27
    E = (pE1 + pE2*dfz)*(1 - (pE3 + pE4 * gammaStar) * sign(alpha))      #4.E24
    Sv = FZ*((pV1 + pV2 * dfz) + (pV3 + pV4 * dfz) * gammaStar)          #4.E29

    FYPurePacejka2002 = D * sin(C * atan(B * alpha -
                                 E * (B * alpha - atan(B * alpha)))) + Sv             #4.E19
}
attr(FYPurePacejka2002, "parameterNames") = c("pC1", "pD1", "pD2", "pD3",
                                              "pE1", "pE2", "pE3", "pE4", "pH1", "pH2", "pH3", "pK1", "pK2",
                                              "pK3", "pV1", "pV2", "pV3", "pV4") #must be alphabetical order
attr(FYPurePacejka2002, "outputName") = "FY"



#' @rdname FYPurePacejka2002
#' @export
#' @family MagicFormulas
FYPureMF52 = FYPurePacejka2002
attr(FYPureMF52, "parameterNames") = c("pC1", "pD1", "pD2", "pD3",
                                       "pE1", "pE2", "pE3", "pE4", "pH1", "pH2", "pH3", "pK1", "pK2",
                                       "pK3", "pV1", "pV2", "pV3", "pV4") #must be alphabetical order
attr(FYPureMF52, "outputName") = "FY"


#' MF5.2 / Pacejka 2002 Pure Longitudinal Magic Formula with IA
#'
#' Uses the Pure Longitudinal Magic Formula (Pacejka 2002). \strong{With a
#'   common modification:} the addition of a \strong{15th parameter, pD3},
#'   to include IA variation, which was not present in the standard 2002
#'   pure longitudinal function. For the unmodified function, see
#'   \code{\link{FXPurePacejka2002.wIA}}. FXPureMF52 is for
#'   Magic Formula 5.2 Pure longitudinal, which is the same as the Pacejka 2002
#'   Pure longitudinal Model. FXPureMF52 function is an alias to
#'   FXPurePacejka2002.wIA.
#'
#' Check out the source code on the tirefittingr github in /R/Magic.R.
#'
#' Works in the SAE Sign conventions: Fz <0, (See Race Car Vehicle Dynamics
#' by Milliken, p39).
#'   or \url{https://www.oreilly.com/library/view/tire-and-vehicle/
#'   9780080970165/images/F150016bm01-9780080970165.jpg}
#'
#' @inheritParams FYPurePacejka2002
#' @param SL numeric. Slip Ratio in SAE standard, unitless.
#'   SL is Slip Ratio based on RE (such that SL=0 gives FX=0).
#'   See Race Car Vehicle Dynamics page 62 (Milliken/Milliken).
#'   For TTC Data, use column named "SL" not "SR".
#' @param parameters numeric Vector. 15 Pacejka Pure-Longitudinal
#'   coefficients in the following order:
#'   pC1, pD1, pD2, pD3, pE1, pE2, pE3, pE4, pK1, pK2, pK3, pV1, pV2,
#'   pH1, pH2
#'
#' @return Longitudinal Force, FX, in Newtons.
#' @export
#'
#' @examples params = c(1,3.16,-0.62,15.59,0.18,3.58,-10.54,-0.25,-0.01,-0.01,
#'   -100,40,2.24,-0.1,-0.45)
#'   FX = FXPurePacejka2002.wIA(
#'       FZ = -1000,
#'       SL = 0.1,
#'       IA = 0,
#'       parameters = params)
#'   #Often used within createFitDataFrameToPlot function to create a plotable
#'   #data frame.
#'   mGridPlot = createFitDataFrame(FXPurePacejka2002.wIA, "parameters",
#'   SL = seq(from = -0.2, to = 0.2, by = 0.005), IA = 0,
#'   FZ = c(c(-250, -750, -1200)),
#'   parameters = params)
#' @references Tyre and Vehicle Dynamics (Pacejka, 2002).
#'   Race Car Vehicle Dynamics (Milliken)
#' @keywords pacejka
#' @family MagicFormulas
FXPurePacejka2002.wIA = function(FZ, SL, IA, FZ0 = -1600, parameters) {

    #parameters must be in alphabetical order.
    pC1 = parameters[1]
    pD1 = parameters[2]
    pD2 = parameters[3]
    pD3 = parameters[4]
    pE1 = parameters[5]
    pE2 = parameters[6]
    pE3 = parameters[7]
    pE4 = parameters[8]
    pH1 = parameters[9]
    pH2 = parameters[10]
    pK1 = parameters[11]
    pK2 = parameters[12]
    pK3 = parameters[13]
    pV1 = parameters[14]
    pV2 = parameters[15]

    gamma = IA*pi/180  #change to rad

    dfz = (FZ - FZ0) / FZ0                                          ###4.E2
    Ksr = FZ * (pK1 + pK2 * dfz) * exp(pK3 * dfz)                         ###4.E15
    C = pC1                                                     ###4.E11
    mu = (pD1 + pD2 * dfz) * (1 - pD3 * gamma^2)                         ###4.E13
    D = mu*FZ                                                   ###4.E12
    B = Ksr / (C * D + 0.001)       #e = 0.001                                      ###4.E16
    E = (pE1 + pE2 * dfz + pE3 * dfz * dfz) * (1 - (pE4 * sign(SL)))         ###4.E14
    Sv = FZ * (pV1 + pV2 * dfz)                                       ###4.E18
    SL = SL + (pH1 + pH2 * dfz)                                       ###4.E10 and 4.E17

    FXPurePacejka2002.wIA = D * sin(C * atan(B * SL -
                                 E * (B * SL - atan(B * SL)))) + Sv  #4.E19
}
attr(FXPurePacejka2002.wIA, "parameterNames") = c("pC1", "pD1", "pD2", "pD3",
                                                  "pE1", "pE2", "pE3", "pE4", "pH1", "pH2", "pK1", "pK2", "pK3", "pV1", "pV2")
#attribute ParameterNames - parameters must be alphabetical order
attr(FXPurePacejka2002.wIA, "outputName") = "FX"


#' @rdname FXPurePacejka2002.wIA
#' @export
#' @family MagicFormulas
FXPureMF52 = FXPurePacejka2002.wIA
attr(FXPureMF52, "parameterNames") = c("pC1", "pD1", "pD2", "pD3",
                                       "pE1", "pE2", "pE3", "pE4", "pH1", "pH2", "pK1", "pK2", "pK3", "pV1", "pV2")
#attribute ParameterNames - parameters must be alphabetical order
attr(FXPureMF52, "outputName") = "FX"


#' Pacejka 2002 Pure Longitudinal Magic Formula
#'
#' Uses the Pure Longitudinal Magic Formula (Pacejka 2002) as written, to
#'   calculate FX. This does not include variation with inclination angle.
#'   Various sources include a 15th parameter, pD3, to include IA variation,
#'   which is not present in this function. To include camber variation, see
#'   \code{\link{FXPurePacejka2002.wIA}}.
#'
#' Check out the source code on the tirefittingr github in /R/Magic.R.
#'
#' Works in the SAE Sign conventions: Fz <0, (See Race Car Vehicle Dynamics
#'   by Milliken, p39).
#'   or \url{https://www.oreilly.com/library/view/tire-and-vehicle/
#'   9780080970165/images/F150016bm01-9780080970165.jpg}
#'
#' @inheritParams FYPurePacejka2002
#' @param SL numeric. Slip Ratio in SAE standard, unitless.
#'   SL is Slip Ratio based on RE (such that SL=0 gives FX=0).
#'   See Race Car Vehicle Dynamics page 62 (Milliken/Milliken).
#'   For TTC Data, use column named "SL" not "SR".
#' @param parameters numeric Vector. 14 Pacejka Pure-Longitudinal
#'   coefficients in the following order:
#'   pC1, pD1, pD2, pE1, pE2, pE3, pE4, pK1, pK2, pK3, pV1, pV2,
#'   pH1, pH2
#'
#' @return Longitudinal Force, FX, in Newtons.
#' @export
#'
#' @examples params = c(1,3.16,-0.62,0.18,3.58,-10.54,-0.25,-0.01,-0.01,
#'   -100,40,2.24,-0.1,-0.45)
#'   FX = FXPurePacejka2002.NoIA(
#'       FZ = -1000,
#'       SL = 0.1,
#'       parameters = params)
#'   #Often used within createFitDataFrameToPlot function to create a plotable
#'   #data frame.
#'   mGridPlot = createFitDataFrame(FXPurePacejka2002.NoIA, "parameters",
#'   SL = seq(from = -0.2, to = 0.2, by = 0.005),
#'   FZ = c(c(-250, -750, -1200)),
#'   parameters = params)
#'
#' @references Tyre and Vehicle Dynamics (Pacejka, 2002)
#'   Race Car Vehicle Dynamics (Milliken)
#' @keywords pacejka
#' @family MagicFormulas
FXPurePacejka2002.NoIA = function(FZ, SL, FZ0 = -1600, Vs = 40, V0 = 40,
                                  parameters) {

    pC1 = parameters[1]
    pD1 = parameters[2]
    pD2 = parameters[3]
    pE1 = parameters[4]
    pE2 = parameters[5]
    pE3 = parameters[6]
    pE4 = parameters[7]
    pH1 = parameters[8]
    pH2 = parameters[9]
    pK1 = parameters[10]
    pK2 = parameters[11]
    pK3 = parameters[12]
    pV1 = parameters[13]
    pV2 = parameters[14]


    dfz = (FZ - FZ0) / FZ0                                              ###4.E2
    Ksr = FZ * (pK1 + pK2 * dfz) * exp(pK3 * dfz)                       ###4.E15
    C = pC1                                                             ###4.E11
    mu = (pD1 + pD2 * dfz) / (1 + Vs/V0)                                ###4.E13
    D = mu*FZ                                                           ###4.E12
    B = Ksr / (C * D + 0.001)       #e = 0.001                          ###4.E16
    E = (pE1 + pE2 * dfz + pE3 * dfz * dfz) * (1 - (pE4 * sign(SL)))    ###4.E14
    Sv = FZ * (pV1 + pV2 * dfz)                                         ###4.E18
    SL = SL + (pH1 + pH2 * dfz)                               ###4.E10 and 4.E17
    FXPurePacejka2002.NoIA = D * sin(C * atan(B * SL -
                                  E * (B * SL - atan(B * SL)))) + Sv    #4.E19
}
attr(FXPurePacejka2002.NoIA, "parameterNames") =
    c("pC1", "pD1", "pD2", "pE1", "pE2", "pE3", "pE4", "pH1", "pH2", "pK1",
      "pK2", "pK3", "pV1", "pV2") #parameters must be alphabetical order
attr(FXPurePacejka2002.NoIA, "outputName") = "FX"

#' Split Tire Data Files
#'
#' Splits Tire data files into multiple data files based on a passed parameter
#'   and saves the new files.
#'
#' Opens each data file from svRunNames, then splits all of the
#'   data into the closest match in the splitting parameter argument.
#'   For example if the fitting parameter was P = c(50,60,80), data points with
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
#'   `getOption(tirefittingr.sfReadTireFile)`. If the option is not set (NULL),
#'   then the default function readTTCData is used, which works for .dat
#'   files from the TTC. For more on this option, see \code{\link{fitTires}}.
#'
#' @param svRunPaths optional string vector. Defaults to NULL.
#'   Complete file path of a tire raw data file. NULL opens a file dialog box
#'   for the user to select files.
#' @param svRunNames optional string vector. Defaults to NULL. String vector the
#'   same length as svRunPaths. Run names for your future reference.
#'   Used as titles for plots. Also gets recorded in summary table. Default NULL
#'   uses the end of the file name.
#' @param ... named argument of a Numeric Vector containing at least two values,
#'   with name corresponding to a column name of a tire data file.
#'   Suggested Names: 'V', 'N', 'SA', 'IA', 'P', 'FZ', 'TSTC', 'SR'.
#' @param  verbose boolean. Defaults to TRUE.
#'   True prints messages in the console, while false does not.
#' @param bSaveBatchNames boolean. Defaults to FALSE. TRUE saves output
#'   to a csv file.
#' @param k integer. Defaults to 10.
#'   Rolling average window width. A rolling
#'   average is applied to the named argument column before splitting.
#'   set to 1 to disable. See k from
#'   \code{\link[zoo:rollmean]{zoo::rollmean()}}
#' @param nRowHeader see rRowHeader from \code{\link{writeFileWithHeader}}
#' @return Returns Dataframe of full-path run names of newly created runs
#'   along with a copy of column 2 from the original input RunNames data frame.
#' @export
#'
#' @examples
#'   \dontrun{
#'   #split into files containing pressures 8, 10, 12, and 14 psi
#'   #with a .dat TTC data file
#'   lRunNamesSplit = splitTireData( P = c(55.2,68.9,82.7,96.5))
#'   lRunNamesSplit = splitTireData( P = 6.894*c(8,10,12,14))
#'   }
#'
#' @importFrom rlang .data
#' @importFrom zoo rollmean
splitTireData = function(svRunPaths = NULL,
                         svRunNames = NULL,
                         ...,
                         verbose = TRUE,
                         bSaveBatchNames = FALSE,
                         k = 10,
                         nRowHeader = 3) {

    svRunPaths = checkTireRunList(svRunPaths) #NULL prompts user to select files
    if (is.null(svRunNames)) svRunNames = basename(svRunPaths)
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
            } else {VarOutput = round(dvSplitVal[j], 1) }

            if (nrow(dfDataPart) > (0.01*dNRowDfData)) {
                #write a new data file
                dSumRows = dSumRows + nrow(dfDataPart)

                sSuffix = paste0("-", VarOutput, "-",sVarName)
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



