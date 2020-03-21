context("testfitting")
library(tirefittingr)

suppressWarnings(library(magrittr))


test_that("Check main fitting function works", {

    expect_Summarydataframe_success <- function(object) {
        # 1. Capture object and label
        act <- quasi_label(rlang::enquo(object), arg = "object")
        # 2. Call expect()
        expect(
            is.data.frame(object),
            sprintf("%s returned from fitTires of type %s, not dataframe/list",
                    act$lab, typeof(object))
        )
        expect(
            object[1,"Status"] == "Fitted",
            sprintf("The first run failed in %s.", act$lab))
        # 3. Invisibly return the value
        invisible(act$val)
    }

    path1 = system.file("extdata", "ABCrun1Lat.dat",
                        package = "tirefittingr", mustWork = TRUE)
    path2 = system.file("extdata", "ABCrun2Lat.dat",
                        package = "tirefittingr", mustWork = TRUE)
    path3 = system.file("extdata", "ABCrun1Long.dat",
                        package = "tirefittingr", mustWork = TRUE)
    path4 = system.file("extdata", "ABCrun2Long.dat",
                        package = "tirefittingr", mustWork = TRUE)


    clearTirefittingrOptions()
    set_test_options()
    options(tirefittingr.testInput = "llaksdjfhl")
    expect_output(fitTires(c(path1, path2)), "Quitting")

    options(tirefittingr.testInput = "Y")
    invisible(capture.output(
    expect_Summarydataframe_success(fitTires(c(path1, path2)))))

    clearTirefittingrOptions()
    set_test_options()
    setFYPure2002()

    #Don't really need this test anymore because the previous one does it
    #expect_Summarydataframe_success(fitTires(c(path1, path2)))

    #FX data input while FY settings are active
    expect_warning(fitTires(c(path3, path4)),
                       "The standard deviation")

    clearTirefittingrOptions()
    set_test_options()
    options(tirefittingr.testInput = "X")
    invisible(capture.output(
        expect_Summarydataframe_success(fitTires(c(path3, path4)))))
    setFXPure2002.wIA()

    #Don't really need this test anymore because the previous one does it
    # c(path3, path4) %>%
    #     fitTires() %>%
    #     expect_Summarydataframe_success()
#
    #FY data input but FX settings are active
    expect_warning(fitTires(c(path1, path2)),
        "A common cause is setting the options for FX, but using FY data")

    setFXPure2002.NoIA()
    c(path3,path4) %>%
        fitTires() %>%
        expect_Summarydataframe_success()

})

