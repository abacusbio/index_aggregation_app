# Purpose : Test cases for the sanity check functions in
#           function_preprocess.R
# Author  : Sameer Atre
#           SAtre@abacusbio.co.nz


# Test target.
# Only needed if your project is not a package.
source("../../R/function_preprocess.R")

# # --------------------------
# # sanityCheckEBVdesc() tests
# # --------------------------
# 
# # 1]
# # Check if the function does its job when the input file
# # just has the required  column_labelling and classifier columns.
#  test_that("sanityCheckEVdesc() works for basic valid data", {
#   txt <- sanityCheckEVdesc(desc_ev, desc_bv)
#   expect_true(is.null(txt)) # a NULL text means everythin's ok !
#  })
# # --------------------------
# # sanityCheckEVdesc() tests
# # --------------------------
# 
# # 1]
# # Check if the function does its job when the input file
# # just has the required  column_labelling and classifier columns.
# test_that("sanityCheckEVdesc() works for basic valid data", {
#   txt <- sanityCheckEVdesc(desc_ev, desc_bv)
#   expect_true(is.null(txt)) # a NULL text means everythin's ok !
# })
# 
# # 2]
# # Check if the function can detect a column header other than
# # column_labelling and classifier
# test_that("sanityCheckEVdesc() detects illegal header", {
#   bad_desc_ev <- rename(desc_ev,
#                         badClassifier = classifier)
#   txt <- sanityCheckEVdesc(bad_desc_ev, desc_bv)
#   expect_false(is.null(txt))
# })
# 
# 
# # 3]
# # Check if the function can return the correct error message
# # if a column header other than column_labelling and classifier is
# # present in the input file.
# test_that("sanityCheckEVdesc() reports illegal header", {
#   bad_desc_ev <- rename(desc_ev,
#                         badClassifier = classifier)
#   txt <- sanityCheckEVdesc(bad_desc_ev, desc_bv)
#   msg <-
#     "Illegal header. Please use 'column_labelling' and 'classifier'.\n"
#   expect_true(grepl(msg, txt))
# })
# 
# 
# # 4]
# # Check if the function can return the correct error message
# # if duplicate headers are detected in the input file.
# test_that("sanityCheckEVdesc() reports duplicate headers", {
#   dup_desc_ev <- desc_ev
#   colnames(dup_desc_ev)[2] <- "column_labelling"
#   txt <- sanityCheckEVdesc(dup_desc_ev, desc_bv)
#   msg <- "Duplicated headers"
#   expect_true(grepl(msg, txt))
# })
# 
# # 5]
# # Check if the function can return the correct error message
# # if column_labelling  does not have a required unique
# # 'Index' ID header in the input file.
# test_that("sanityCheckEVdesc() checks 'Index' exists", {
#   no_index_desc_ev <- subset(desc_ev, column_labelling != "Index")
#   txt <- sanityCheckEVdesc(no_index_desc_ev, desc_bv)
#   msg <- "At least one unique ID header has to be 'Index'"
#   expect_true(grepl(msg, txt))
# })
# 
# 
# # 6]
# # Check if the function can return the correct error message
# # when one of the required classifiers are missing
# # in the input file. Required classifers are ID (for ID),
# # and EV (for trait EV).
# test_that("sanityCheckEVdesc() checks if required classifiers exist", {
#   no_cls_desc_ev <- subset(desc_ev, !classifier %in% c("EV"))
#   txt <- sanityCheckEVdesc(no_cls_desc_ev, desc_bv)
#   msg <- "Missing classifier"
#   expect_true(grepl(msg, txt))
# })
# 
# 
# # 7]
# # Check if the function can return the correct error message
# # when one the classifiers are not one of ID (for ID),
# # ClassVar (for categorical variables), EV (for trait EV) and
# # Group (group assignment for traits).
# test_that("sanityCheckEVdesc() checks for unknown classifiers", {
#   bad_cls_ev <- desc_ev
#   bad_cls_ev$classifier[bad_cls_ev$column_labelling == 'TARSPOT'] = 'GEV'
#   txt <- sanityCheckEVdesc(bad_cls_ev, desc_bv)
#   msg <- "Unknown classifier"
#   expect_true(grepl(msg, txt))
# })
# 
# 
# # 8]
# # Check if the function can return the correct error message
# # when the column_labelling contents in the input file are not
# # identical to those in the EBV description file.
# test_that(
#   "sanityCheckEVdesc() checks if trait(s) in the EV description file
#    matches with those in the EBV description file",
#   {
#     txt <- sanityCheckEVdesc(desc_ev_tr_mismatch, desc_bv)
#     msg <-
#       "Trait(s) in EV description doesn't exist in EBV description:"
#     expect_true(grepl(msg, txt, fixed = TRUE))
#   }
# )
# 
# # --------------------------
# # sanityCheckEBV() tests
# # --------------------------
# 
# # 1]
# # Check if the function does its job when the input file
# # just has the required columns in the correct format.
# test_that("sanityCheckEBV() works for basic valid data", {
#   txt <- sanityCheckEBV(bv,desc_bv)
#   expect_true(is.null(txt)) # a NULL text means everythin's ok !
# })
# 
# 
# # 2]
# # Check if the function can return the correct error message
# # if duplicate headers are detected in the input file.
# test_that("sanityCheckEBV() reports duplicate headers", {
#   dup_bv <- bv
#   colnames(dup_bv)[5] <- "ANTHRAC"
#   txt <- sanityCheckEBV(dup_bv, desc_bv)
#   msg <- "Duplicated headers"
#   expect_true(grepl(msg, txt))
# })
# 
# 
# # 3]
# # # Check if the function can return the correct error message
# # # when the list of column headers in the input file is not
# # # identical to that in the EBV description file.
# test_that("sanityCheckEBV() checks if the list of trait(s) in the 
#     EBV data file matches with that in the EBV description file", {
#   tr_mismatch_bv <- bv[ , !names(bv) %in% "TARSPOT"]
#   txt <- sanityCheckEBV(tr_mismatch_bv, desc_bv)
#   msg <- "Breeding value column header do not match file description"
#   expect_true(grepl(msg, txt))
# })
# 
# 
# # 4]
# # Check if the function can return the correct error messag
# # when column headers in the input file are not present
# # in the EBV description file.
# test_that("sanityCheckEBV() checks if traits in the EBV data file
#     are present in the EBV description file", {
#       no_desc_trait_bv <- bv
#       no_desc_trait_bv$ANTHRAC_NZ = bv$ANTHRAC
#       txt <- sanityCheckEBV(no_desc_trait_bv, desc_bv)
#       msg <- "does not exist in description file"
#       expect_true(grepl(msg, txt))
#     })
# 
# 
# # 5]
# # Check if the function can return the correct error message
# # when the input file contains character strings instead of numeric types.
# test_that("sanityCheckEBV() checks if traits values in the EBV data file
#     are character strings instead of numbers", {
#       char_trait_bv <- bv
#       char_trait_bv[, 4] <- sapply(char_trait_bv[, 4], as.character)
#       txt <- sanityCheckEBV(char_trait_bv, desc_bv)
#       msg <- "Character strings detected"
#       expect_true(grepl(msg, txt))
#     })


# -------------------------
# sanityCheckEV() tests
# -------------------------

# 1]
# Check if the function does its job when the input file just has
# the required  columns in the correct format.
test_that("sanityCheckEV() works for basic valid data", {
  txt <- sanityCheckEV(ev, desc_ev_match)
  expect_true(is.null(txt)) # a NULL text means everythin's ok !
})


# 2]
# Check if the function can return the correct error message when
# the first column in the input file is not 'Index'.
test_that(
  "sanityCheckEV() checks if first column is Index",
  {
    no_first_index_ev <- ev
    no_first_index_ev <-
      no_first_index_ev %>% relocate(Index, .after = TARSPOT)
    txt <- sanityCheckEV(no_first_index_ev, desc_ev_match)
    msg <- "First column name should be 'Index'"
    expect_true(grepl(msg, txt))

})


# 3]
# Check if the function can return the correct error message when
# the 'Indices' are not unique in the input file.
test_that(
  "sanityCheckEV() checks if 'Index' column is duplicated",
  {
    txt <- sanityCheckEV(ev_dup_index, desc_ev_match)
    msg <- "Duplicated indexes"
    expect_true(grepl(msg, txt))

  })

# 4]
# Check if the function can return the correct error message when
# a column header is duplicated in the input file.
test_that("sanityCheckEV() checks if a column header is duplicated",
  {
    txt <- sanityCheckEV(ev_dup_hdr, desc_ev_match)
    msg <- "Duplicated headers"
    expect_true(grepl(msg, txt))

  })


# 5]
# Check if the function can return the correct error message when
# the list of column headers in the input file does not match with
# those in the description file.
test_that(
  "sanityCheckEV() checks for mismatch between the column
          headers in the input and description files",
  {
    txt <- sanityCheckEV(ev, desc_ev)
    msg <-
      "file description column headers do not match data file"
    expect_true(grepl(msg, txt))

  }
)


# 6]
# Check if the function can return the correct error message when
# column headers in the input file do not exist within
# the description file.
test_that(
  "sanityCheckEV() checks if there is a column header in the input file
      that does not exist in the description file",
  {
    txt <- sanityCheckEV(ev_invalid_hdr, desc_ev_match)
    msg <-
      "does not exist in description file"
    expect_true(grepl(msg, txt))

  }
)

