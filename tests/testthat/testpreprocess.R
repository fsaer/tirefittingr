context("Preprocess")
library(tirefittingr)
library(magrittr)

test_that("Check checkTireRunList works", {

    path1 = system.file("extdata", "ABCrun1Lat.dat",
                        package = "tirefittingr", mustWork = TRUE)
    path2 = system.file("extdata", "ABCrun2Lat.dat",
                        package = "tirefittingr", mustWork = TRUE)
    path3 = system.file("extdata", "ABCrun1Long.dat",
                        package = "tirefittingr", mustWork = TRUE)
    path4 = system.file("extdata", "ABCrun2Long.dat",
                        package = "tirefittingr", mustWork = TRUE)

    expect_equal(checkTireRunList(path1),path1)

    expect_equal(checkTireRunList(c(path1, path2)), c(path1,path2))

    # testObject = "test"
    # expect_warning(checkTireRunList("./data/ABCrun1Lat.dat", "testObject"),
    #                "are available by default.")
    pathTemp = "ABCrun1Lat.testfilext"
    expect_warning(checkTireRunList(pathTemp), "extension")

    pathTemp = "DOESNTEXIST_sdkfjasldfkj.dat"
    expect_warning(checkTireRunList(pathTemp), "does not exist")
})

test_that("Reading TTC Data Works",{

    # path = "ABCrun1Lat.Rdata"
    path1 = system.file("extdata", "ABCrun1Lat.dat",
                package = "tirefittingr", mustWork = TRUE)

    expect_true(sum(readTTCData(path1) - ABCrun1LatPreProccessed) < 1e-6)
})

test_that("Split Data Function Works",{

    pathReadSplit = system.file("extdata", "ABCrunLat.dat",
                                  package = "tirefittingr", mustWork = TRUE)

    pathReadBaseline = system.file("extdata", "ABCrun1Lat.dat",
                                   package = "tirefittingr", mustWork = TRUE)

    if (!file.exists(pathReadSplit)) {
        stop("File Not found: ", normalizePath(pathReadSplit))
    }

    expect_error(splitTireData(pathReadSplit,
                             P = c(55.2,68.9,82.7,96.5),
                             FZ = c(11,32),
                             verbose = FALSE,
                             bSaveBatchNames = FALSE),
                 "fSplitData Can only handle ONE variable to split by")

    dfSplitDataRuns = splitTireData(pathReadSplit,
                                    P = c(55.2,68.9,82.7,96.5),
                                    verbose = FALSE,
                                    bSaveBatchNames = FALSE)

    dfSplit = readTTCData(dfSplitDataRuns[1,1])
    dfBaseline = readTTCData(pathReadBaseline)

    expect_true( nrow(dfSplit) > 400)
    expect_true( (nrow(dfBaseline) - nrow(dfSplit)) < 20 )

    unlink(dfSplitDataRuns[1,1]) #delete the split files
    unlink(dfSplitDataRuns[2,1])

    sCSVPath = system.file("extdata", "ABCrun1Lat.csv",
                             package = "tirefittingr", mustWork = TRUE)
    expect_equal(
        readCSVTireData(sCSVPath),
        read.csv(sCSVPath, stringsAsFactors = FALSE))
})

test_that("checkRequiredOption Function Works",{
    clearTirefittingrOptions()
    options(tirefittingr.testthisfunction = "anObjectThatDoesntExist3i123479")
    expect_error(
        checkRequiredOption("tirefittingr.testthisfunction", bMustExist = TRUE),
        "does not point to an object that exists")
})
