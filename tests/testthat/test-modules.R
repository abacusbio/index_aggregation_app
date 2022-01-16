#' Test-modules.R
#'
#' Test cases for generic module servers. 
#' @author Sameer Atre <SAtre@@abacusbio.co.nz>


#' test_file("test-modules.R", reporter = c("Check","Location"))

source("../../R/modules.R")

# Test data
# For data formats and required fields see the index testing app at
# https://abacusbio.shinyapps.io/selection_index_revamp/

# --------------------
# User uploaded files
# --------------------
usr_desc_ebv <- "../../R/data/description_bv.xlsx"
usr_desc_ev <-  "../../R/data/description_ev.csv"
usr_dat_ebv <-  "../../R/data/bv.csv"
usr_dat_ev <- "../../R/data/ev.csv"
usr_dat_w <-  "../../R/data/index_weight.csv"

desc_ebv <-
  readxl::read_xlsx(usr_desc_ebv, col_names = T)
desc_ev <- read.csv2(
  usr_desc_ev,
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
  colClasses = c("character", rep("numeric", 11), rep("character", 2)),
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
# (Step 1 file uploads) : Checks that the uploadTable module responds correctly 
# when the user uploads a data file.
test_that("uploadTable module responds correctly when a data file is uploaded",
          {
            testServer(uploadTableModuleServer, args = list(id = "desc_ebv"), {
              session$setInputs(file = list(name = "description_bv.xlsx",
                                            datapath = usr_desc_ebv))
              session$flushReact()
              expect_equal(userFile()$datapath, usr_desc_ebv)
              expect_equal(session$returned(), desc_ebv)
            })
            
            testServer(uploadTableModuleServer, args = list(id = "desc_ev"), {
              session$setInputs(file = list(name = "description_ev.xlsx",
                                            datapath = usr_desc_ev))
              session$flushReact()
              expect_equal(userFile()$datapath, usr_desc_ev)
              expect_equal(session$returned(), desc_ev)
            })
            
          })
