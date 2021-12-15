# Purpose : Test cases for the sanity check functions in
#           function_preprocess.R
# Author  : Sameer Atre
#           SAtre@abacusbio.co.nz


# We want a clean test report with less clutter.
suppressWarnings({
  suppressPackageStartupMessages({
    library(testthat)
    library(readxl)
    library(tidyverse)
  })
})

# Test target.
# Only needed if your project is not a package.
source("../../R/function_preprocess.R")

# --------------------------
# sanityCheckEBVdesc() tests
# --------------------------

# 1]
# Check if the function does its job when the input file
# just has the required  column_labelling and classifier columns.
test_that("sanityCheckEBVdesc() works for basic valid data", {
  txt <- sanityCheckEBVdesc(desc_bv)
  expect_true(is.null(txt)) # a NULL text means everythin's ok !
})

# 2]
# Check if the function can parse data with optional columns.
# when the input file only has the required  column_labelling and
# classifier columns. Optional columns are unit,order and group.
test_that("sanityCheckEBVdesc() works for optional valid data", {
  txt <- sanityCheckEBVdesc(desc_bv_opt)
  expect_true(is.null(txt))
})

# 3]
# Check if the function can detect a column header other than
# column_labelling,classifier,unit,order and group.
test_that("sanityCheckEBVdesc() detects illegal header", {
  new_desc_ebv <- rename(desc_bv,
                         badClassifier = classifier)
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  expect_false(is.null(txt))
})

# 4]
# Check if the function can return the correct error message
# if a column header other than column_labelling,classifier,
# unit,order,group is detected in the input file.
test_that("sanityCheckEBVdesc() reports illegal header", {
  new_desc_ebv <- rename(desc_bv,
                         badClassifier = classifier)
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  msg <-
    "Illegal header. Please use 'column_labelling' and 'classifier'.\n"
  expect_true(grepl(msg, txt))
})

# 5]
# Check if the function can return the correct error message
# if one of the required column headers (column_labelling and classifier)
# are missing in the input file.
test_that("sanityCheckEBVdesc() detects missing header", {
  new_desc_ebv <- desc_bv
  names(new_desc_ebv) <- NULL
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  msg <-
    "Illegal header. Please use 'column_labelling' and 'classifier'.\n"
  expect_true(grepl(msg, txt))
})

# 6]
# Check if the function can return the correct error message
# if duplicate headers are detected in the input file.
test_that("sanityCheckEBVdesc() detects duplicate headers", {
  txt <- sanityCheckEBVdesc(dup_desc_bv)
  msg <- " Duplicated headers "
  expect_true(grepl(msg, txt))
})

# 7]
# Check if the function can return the correct error message
# if column_labelling  does not have a required unique ID field
# in the input file.
test_that("sanityCheckEBVdesc() checks 'ID' exists", {
  no_ID_desc_ebv <- subset(desc_bv, classifier != "ID")
  txt <- sanityCheckEBVdesc(no_ID_desc_ebv)
  msg <- "At least one unique ID header has to be 'ID'"
  expect_true(grepl(msg, txt))
})

# 8]
# Check if the function can return the correct error message
# when one of the required classifiers are missing
# in the input file. Required classifers are ID (for ID),
# ClassVar (for categorical variables), EBV (for trait EBV) and
# ACC (for trait accuracies).
test_that("sanityCheckEBVdesc() checks classifiers exist", {
  no_cls_desc_ebv <- subset(desc_bv,!classifier %in% c("EBV"))
  txt <- sanityCheckEBVdesc(no_cls_desc_ebv)
  msg <- " Missing classifier:\n"
  expect_true(grepl(msg, txt))
})

# 9]
# Check if the function can return the correct error message
# when one the classifiers are not one of ID (for ID),
# ClassVar (for categorical variables), EBV (for trait EBV) and
# ACC (for trait accuracies).
test_that("sanityCheckEBVdesc() checks for unknown classifiers", {
  new_desc_ebv <- desc_bv
  new_desc_ebv$classifier[new_desc_ebv$column_labelling == 'MOIST'] = 'GBV'
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  msg <- " Unknown classifier:\n"
  expect_true(grepl(msg, txt))
})

# 10]
# Check if the function can return the correct error message
# when an ACC classifier does not have a corresponding 'TraitACC' entry
# in the column_labelling colum of the input file.
test_that("sanityCheckEBVdesc() checks for Accuracy-Trait mismatch", {
  txt <- sanityCheckEBVdesc(test_desc_bv)
  msg <- " Accuracy names do not match trait names"
  expect_true(grepl(msg, txt))
})

# 11]
# Check if the function can return the correct error message
# when there are identical trait ordering values in the input file.
# Trait ordering should be unique as they assign the order of traits
# to show in tables and plots.
test_that("sanityCheckEBVdesc() checks for identical order numbers", {
  txt <- sanityCheckEBVdesc(desc_bv_identical_odr)
  msg <- "there are identical order numbers"
  expect_true(grepl(msg, txt))
})

# 12]
# Check if the function can return the correct error message
# when there are missing ordering values for trait label/EBV rows
# in the input file. While the order column is optional, every
# trait label/EBV row should have an order value if the order
# column is present.
test_that("sanityCheckEBVdesc() checks for missing order numbers", {
  txt <- sanityCheckEBVdesc(desc_bv_missing_odr)
  msg <- "Missing order for trait(s)"
  expect_true(grepl(msg, txt))
})


# 13]
# Check if the function can return the correct error message
# when there are missing group assignment for traits in the input file.
# While the group column is optional, every
# trait label/EBV row should have a group value if the group
# column is present in the input file.
test_that("sanityCheckEBVdesc() checks for missing group numbers", {
  txt <- sanityCheckEBVdesc(desc_bv_missing_grp)
  msg <- "Missing group for trait(s)"
  expect_true(grepl(msg, txt))
})

# --------------------------
# sanityCheckEVdesc() tests
# --------------------------
