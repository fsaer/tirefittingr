

#' Data Frame of Starting Population for fitting FYPurePacejka2002
#'
#' Starting population is 180 sets of randomly generated coefficients that
#'   provide a starting point for the genetic solver. The minimum and maximum
#'   limits of each parameter in the solution are taken from the minimum
#'   and maximum values in this data frame.
#'
#' @format A data frame
"dfStartParFY"

#' Data Frame of Starting Population for fitting FXPurePacejka2002
#'
#' Starting population is 150 sets of randomly generated coefficients that
#'   provide a starting point for the genetic solver. The minimum and maximum
#'   limits of each parameter in the solution are taken from the minimum
#'   and maximum values in this data frame. Population works with both
#'   FXPurePacejka2002.wIA and FXPurePacejka2002.NoIA.
#'
#' @format A data frame
"dfStartParFX"

#' tirefittingr built in datasets
#'
#' Fake tire data made from randomly generated parameters
#' @format A data frame with 1451 rows and 8 variables:
#' \describe{
#'   \item{SA}{Slip Angle in degrees}
#'   \item{IA}{Inclination Angle in degrees}
#'   \item{FZ}{Normal Load in N}
#'   \item{FY}{Lateral Load in N}
#'   \item{TSTC}{Temperature in C}
#'   \item{FX}{Longitudinal Load in N}
#'   \item{SR}{Slip Ratio (unitless)}
#'   \item{P}{Pressure in kPa}
#' }
"ABCrun1LatPreProccessed"

#' tirefittingr built in datasets
#'
#' Fake tire data made from randomly generated parameters
#' @format A data frame with 1451 rows and 8 variables:
#' \describe{
#'   \item{SA}{Slip Angle in degrees}
#'   \item{IA}{Inclination Angle in degrees}
#'   \item{FZ}{Normal Load in N}
#'   \item{FY}{Lateral Load in N}
#'   \item{TSTC}{Temperature in C}
#'   \item{FX}{Longitudinal Load in N}
#'   \item{SR}{Slip Ratio (unitless)}
#'   \item{P}{Pressure in kPa}
#' }
"ABCrun2LatPreProccessed"

#' tirefittingr built in datasets
#'
#' Fake tire data made from randomly generated parameters
#' @format A data frame with 606 rows and 8 variables:
#' \describe{
#'   \item{SL}{Slip Ratio (unitless)}
#'   \item{IA}{Inclination Angle in degrees}
#'   \item{FZ}{Normal Load in N}
#'   \item{FX}{Longitudinal Load in N}
#'   \item{SA}{Slip Angle in degrees}
#'   \item{TSTC}{Temperature in C}
#'   \item{FY}{Lateral Load in N}
#'   \item{P}{Pressure in kPa}
#' }
"ABCrun1LongPreProccessed"

#' tirefittingr built in datasets
#'
#' Fake tire data made from randomly generated parameters
#' @format A data frame with 606 rows and 8 variables:
#' \describe{
#'   \item{SL}{Slip Ratio (unitless)}
#'   \item{IA}{Inclination Angle in degrees}
#'   \item{FZ}{Normal Load in N}
#'   \item{FX}{Longitudinal Load in N}
#'   \item{SA}{Slip Angle in degrees}
#'   \item{TSTC}{Temperature in C}
#'   \item{FY}{Lateral Load in N}
#'   \item{P}{Pressure in kPa}
#' }
"ABCrun2LongPreProccessed"

### NOTE: Updates to the datasets that are included in the package are generated
# and saved in the NoPublish.R script within the /R folder.
