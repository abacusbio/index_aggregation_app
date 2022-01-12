#' Test-module_preprocess.R
#'
#' Test cases for the sanity check functions in function_preprocess.R
#' @author Sameer Atre <SAtre@@abacusbio.co.nz>


#' test_file("test-module_preprocess.R", reporter = c("Check","Location"))

# Test targets and their references.
# Only needed if your project is not a package.
source("../../R/module_preprocess.R")
source("../../R/modules.R")

# checks that the module responds correctly as the reactive input changes
test_that("output updates when reactive input changes", {
  testVal <- reactiveValues()
  testServer(preprocessUploadMod, args = list(id = "step1", val = testVal), {
    session$setInputs(help_btn = 2)
    # session$flushReact()
    expect_equal(
      output$sanity_message,
      "Choose demo or upload your own files",
    )
  })
})

test_that("output updates when reactive input changes", {
  testVal <- reactiveValues()
  testVal$desc_ebv <- demo_desc_ebv
  testVal$desc_ev  <- demo_desc_ev
  testVal$dat_ebv  <- demo_dat_ebv
  testVal$dat_ev <- demo_dat_ev
  testVal$dat_w <- demo_dat_w
  
  testServer(preprocessUploadMod, args = list(id = "step1", val = testVal), {
    session$setInputs(help_btn = 2)
    # session$flushReact()
    expect_equal(
      output$sanity_message,
      "All good. Now you can move to Results, or Filter if you want to subset your inputs.",
    )
  })
})
