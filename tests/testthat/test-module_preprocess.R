#' Test-module_preprocess.R
#'
#' Test cases for the preprocess upload module server
#' @author Sameer Atre <SAtre@@abacusbio.co.nz>


#' test_file("test-module_preprocess.R", reporter = c("Check","Location"))

# Test targets and their referred modules.
# Only needed if your project is not a package.
source("../../R/module_preprocess.R")
source("../../R/modules.R")

# Test data
# For data formats and required fields see the index testing app at
# https://abacusbio.shinyapps.io/selection_index_revamp/

# ------------------
# Demo data files
# ------------------
demo_desc_ebv <-
  readxl::read_xlsx("../../R/data/description_bv.xlsx", col_names = T)

demo_desc_ev <- read.csv2(
  "../../R/data/description_ev.csv",
  sep = ",",
  col.names = c("column_labelling", "classifier")
)

demo_dat_ebv <- read.table(
  "../../R/data/bv.csv",
  colClasses = c(rep("character", 2), rep("double", 14)),
  header = T,
  sep = ",",
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = F,
  quote = "\"",
  fill = T,
  comment.char = "",
  dec = ".",
  check.names = F,
  strip.white = T
)

demo_dat_ev <- read.table(
  "../../R/data/ev.csv",
  colClasses = c("character", rep("double", 11), rep("character", 2)),
  header = T,
  sep = ",",
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = F,
  quote = "\"",
  fill = T,
  comment.char = "",
  dec = ".",
  check.names = F,
  strip.white = T
)

demo_dat_w <- read.table(
  "../../R/data/index_weight.csv",
  colClasses = c("character", "double"),
  header = T,
  sep = ",",
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = F,
  quote = "\"",
  fill = T,
  comment.char = "",
  dec = ".",
  check.names = F,
  strip.white = T
)


# 1]
# Demo data : Checks that the module responds correctly as user clicks
# the 'Run demo data' button.
test_that("clicking 'Run demo data button' loads demo data", {
  # User hasn't uploaded any data files or clicked the demo button.
  testVal <- reactiveValues()
  testServer(preprocessUploadMod, args = list(id = "step1", val = testVal), {
    msg <- "Choose demo or upload your own files"
    expect_true(grepl(msg, output$sanity_message))

    # Test clicking the demo button.
    testVal$desc_ebv <- demo_desc_ebv
    testVal$desc_ev  <- demo_desc_ev
    testVal$dat_ebv  <- demo_dat_ebv
    testVal$dat_ev <- demo_dat_ev
    testVal$dat_w <- demo_dat_w
    session$flushReact()

    msg <- "You are using demo data now"
    expect_true(grepl(msg, output$demo_message))
    msg <- "All good. Now you can move to Results"
    expect_true(grepl(msg, output$sanity_message))
  })
  
})



