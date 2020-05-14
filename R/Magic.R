
#' MF5.2 / Pacejka 2002 Pure Lateral Magic Formula
#'
#' Calculates the lateral tire force given FZ, SA, IA, and a set of parameters
#'   using the Pacejka 2002 pure lateral formula. FYPureMF52 is for
#'   Magic Formula 5.2 Pure lateral, which is the same as the Pacejka 2002 Pure
#'   Lateral Model. FYPureMF52 function is an alias to FYPurePacejka2002.
#'
#' Check out the source code on the tirefittingr github in /R/Magic.R.
#'
#' Works in the SAE Sign conventions: Fz <0, (See Race Car Vehicle Dynamics
#' by Milliken, p39).
#'   or \url{https://www.oreilly.com/library/view/tire-and-vehicle/9780080970165/images/F150016bm01-9780080970165.jpg}
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
#' @param Vs numeric. Tire Slip velocity in kph. Defaults to 0 kph.
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
#'   Set to 1 during fitting. Testing from a facility (e.g. TTC) is
#'   done on a sandpaper "road" which has a higher coefficient of friction
#'   (mu) than regular pavement. To use this function to estimate real forces
#'   that would be seen on pavement (for lap sim, etc), mu must be scaled
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
FYPurePacejka2002 = function(FZ, SA, IA, Vs = 0,
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
#'   \code{\link{FXPurePacejka2002.wIA}}. `FXPureMF52` is for
#'   Magic Formula 5.2 Pure longitudinal, which is the same as the Pacejka 2002
#'   Pure longitudinal Model. `FXPureMF52` function is an alias to
#'   `FXPurePacejka2002.wIA`.
#'
#' Check out the source code on the tirefittingr github in /R/Magic.R.
#'
#' Works in the SAE Sign conventions: Fz <0, (See Race Car Vehicle Dynamics
#' by Milliken, p39).
#'   or \url{https://www.oreilly.com/library/view/tire-and-vehicle/9780080970165/images/F150016bm01-9780080970165.jpg}
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
    #pH3 unused
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
#'   or \url{https://www.oreilly.com/library/view/tire-and-vehicle/9780080970165/images/F150016bm01-9780080970165.jpg}
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
FXPurePacejka2002.NoIA = function(FZ, SL, FZ0 = -1600, Vs = 0, V0 = 40,
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
    #pH3 unused
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
