context("Postprocess")
library(tirefittingr)
library(magrittr)

test_that("Check Plot Goodness works", {

    options(
        tirefittingr.sSavePlotPath = FALSE
    )

    parameters = c(20,5.89,-1.23,-12.23,1.87,-0.75,0.08,90.44,0,0.02,
                   -0.18,187.77,11.81,-23.34,-0.04,-0.14,0.42,0.02)

    data = createFitDataFrame(
          FYPurePacejka2002,
          sParameterArgName = "parameters",
          SA = seq(-12,12,0.25),
           IA = 0,
          FZ = c(-250, -500, -750, -1000, -1500),
          parameters = parameters)

    lPlotSetup = list(
        fFitFunction = FYPurePacejka2002,
        iDataPointsPlot = 12000,
        sInCol = "SA",
        sOutCol = "FY",
        sSweep = "FZ",
        lConst = list(IA = 0, FZ0 = -1600),
        xlab = "SA (deg)",
        ylab = "Fy (N)",
        sSavePath = NULL)
    expect_equal(
        plotGoodnessOfFit(data, parameters, lPlotSetup),
        "Plot Succeeded")

})


test_that("Check that plot run conditions works", {

    parameters = c(20,5.89,-1.23,-12.23,1.87,-0.75,0.08,90.44,0,0.02,
                   -0.18,187.77,11.81,-23.34,-0.04,-0.14,0.42,0.02)

    data = createFitDataFrame(
        FYPurePacejka2002,
        sParameterArgName = "parameters",
        SA = seq(-12,12,0.25),
        IA = 0,
        FZ = c(-250, -500, -750, -1000, -1500),
        parameters = parameters)

    options(
        tirefittingr.bPlotRunConditions = TRUE
    )

    #Tests with plotly
    expect_equal(
        plotRunConditions(data, "Test Plot", sSavePath = NULL),
        "Plot Run Conditions Successful")

    #Tests with graphics::plot
    expect_equal(
        plotRunConditions(data, "Test Plot", sSavePath = tempfile()),
        "Plot Run Conditions Successful")
})

