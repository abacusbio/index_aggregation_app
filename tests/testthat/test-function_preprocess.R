suppressWarnings({

library(readxl)
library(tidyverse)

desc_ebv <- read_xlsx("description_bv.xlsx", 
                      sheet = 1, 
                      skip = 0,
                      col_types = rep("text", 2),
                      col_names = T, 
                      trim_ws = T )
  
test_that("sanityCheckEBVdesc() works for valid data", {
  txt <- sanityCheckEBVdesc(desc_ebv)
  expect_true(is.null(txt))
})

test_that("sanityCheckEBVdesc() detects illegal header", {
  new_desc_ebv <- rename(desc_ebv, 
                         badClassifier = classifier)
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  expect_false(is.null(txt))
})

test_that("sanityCheckEBVdesc() reports illegal header", {
  new_desc_ebv <- rename(desc_ebv, 
                         badClassifier = classifier)
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  msg <- "Illegal header. Please use 'column_labelling' and 'classifier'.\n"
  expect_true(grepl(msg,txt))
})

test_that("sanityCheckEBVdesc() detects missing header", {
  new_desc_ebv <- desc_ebv
  names(new_desc_ebv) <- NULL
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  msg <- "Illegal header. Please use 'column_labelling' and 'classifier'.\n"
  expect_true(grepl(msg,txt))
})

test_that("sanityCheckEBVdesc() detects duplicate headers", {
  dup_desc_ebv <- read_xlsx("description_bv_duplicate_hdr.xlsx", 
                        sheet = 1, 
                        skip = 0,
                        col_types = rep("text", 2),
                        col_names = T, 
                        trim_ws = T )
  txt <- sanityCheckEBVdesc(dup_desc_ebv)
  print(txt)
  msg <- " Duplicated headers "
  expect_true(grepl(msg,txt))
})

test_that("sanityCheckEBVdesc() checks 'ID' exists", {
  no_ID_desc_ebv <- subset(desc_ebv, classifier!="ID")
  txt <- sanityCheckEBVdesc(no_ID_desc_ebv)
  msg <- "At least one unique ID header has to be 'ID'"
  expect_true(grepl(msg,txt))
})

test_that("sanityCheckEBVdesc() checks classifiers exist", {
  no_cls_desc_ebv <- subset(desc_ebv, 
                           !classifier %in% c("EBV"))
  txt <- sanityCheckEBVdesc(no_cls_desc_ebv)
  msg <- " Missing classifier:\n"
  expect_true(grepl(msg,txt))
})

test_that("sanityCheckEBVdesc() checks for unknown classifiers", {
  new_desc_ebv <- desc_ebv
  new_desc_ebv$classifier[new_desc_ebv$column_labelling=='MOIST'] = 'GBV'
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  msg <- " Unknown classifier:\n"
  expect_true(grepl(msg,txt))
})

})