#' Test-function_preprocess.R
#'
#' Unit test cases for the sanity check functions in function_preprocess.R
#' @author Sameer Atre <SAtre@@abacusbio.co.nz>


#' test_file("test-function_preprocess.R",reporter = c("Check","Location"))

# Test target.
# Only needed if your project is not a package.
source("../../R/function_preprocess.R")

# Test data
# For data formats and required fields see the index testing app at
# https://abacusbio.shinyapps.io/selection_index_revamp/

# ----------------------------------
# Breeding value file descriptions
# ----------------------------------

# Normal'good'data.Has just the required columns and values.
desc_bv <- read_xlsx(
  "../data/desc_bv.xlsx",
  sheet = 1,
  skip = 0,
  col_types = rep("text", 2),
  col_names = T,
  trim_ws = T
)

# Normal'good'data but with optional fields.
suppressMessages(desc_bv_opt <-
                   read_csv("../data/desc_bv_opt.csv"))


# Has duplicate column headers.
dup_desc_bv <- read_xlsx(
  "../data/desc_bv_dup_header.xlsx",
  sheet = 1,
  skip = 0,
  col_types = rep("text", 2),
  col_names = T,
  trim_ws = T
)

# ACC-Trait mismatch.
# Whereas every 'ACC' classifier should have a corresponding
# 'TraitACC' entry in the column_label, this file has an ACC
# classifier entries but does not have corresponding TraitACC
# column_labels.
test_desc_bv <-
  read_xlsx(
    "../data/desc_bv_mismatch_acc_trait.xlsx",
    sheet = 1,
    skip = 0,
    col_types = rep("text", 2),
    col_names = T,
    trim_ws = T
  )

# Has identical order values in trait label/EBV rows.
suppressMessages(desc_bv_identical_order <- read_csv(
  "../data/desc_bv_identical_order.csv")
)

# Has missing order values in trait label/EBV rows.
suppressMessages(desc_bv_missing_order <- read_csv(
  "../data/desc_bv_missing_order.csv")
)

# Has missing group values in trait label/EBV rows.
suppressMessages(desc_bv_missing_group <- read_csv(
  "../data/desc_bv_missing_group.csv")
)


# ----------------------------------
# Economic value file descriptions
# ----------------------------------

# Normal'good'data.Has just the required columns and values.
suppressMessages(desc_ev <- read_csv(
  "../data/desc_ev.csv")
)

# Column_labelling contents are not identical to those in the 
# EBV description file.
suppressMessages(desc_ev_mismatch_trait <- read_csv(
  "../data/desc_ev_mismatch_trait.csv")
)

# -----------------
# Breeding values
# -----------------
# Normal'good'data.Has just the required columns and values.
suppressMessages(
  bv <- read.table(
    "../data/bv.csv",
    header = T,
    colClasses = c("numeric", "character", rep("double", 14)),
    sep = ",",
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = F,
    quote = "\"",
    fill = T,
    comment.char = "",
    dec = ".",
    check.names = F,
    strip.white = T,
  )
)

# -----------------
# Economic values
# -----------------

suppressMessages({
  # Normal'good'data.Has just the required columns and values.
  ev <- read.table(
    "../data/ev.csv",
    header = T,
    colClasses = c("character", rep("double", 10), rep("character", 2)),
    sep = ",",
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = F,
    quote = "\"",
    fill = T,
    comment.char = "",
    dec = ".",
    check.names = F,
    strip.white = T,
  )
  
  # 'Index' column is duplicated.
  ev_dup_header <- read.table(
    "../data/ev_dup_header.csv",
    header = T,
    colClasses = c("character", rep("double", 10), rep("character", 3)),
    sep = ",",
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = F,
    quote = "\"",
    fill = T,
    comment.char = "",
    dec = ".",
    check.names = F,
    strip.white = T,
  )
  
  # Indices are not unique.
  ev_dup_index <- read.table(
    "../data/ev_dup_index.csv",
    header = T,
    colClasses = c("character", rep("double", 10), rep("character", 2)),
    sep = ",",
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = F,
    quote = "\"",
    fill = T,
    comment.char = "",
    dec = ".",
    check.names = F,
    strip.white = T,
  )
  
  # Currently unused.
  ev_mismatch_header <- read.table(
    "../data/ev_mismatch_header.csv",
    header = T,
    colClasses = c("character", rep("double", 11), rep("character", 2)),
    sep = ",",
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = F,
    quote = "\"",
    fill = T,
    comment.char = "",
    dec = ".",
    check.names = F,
    strip.white = T,
  )
  
  # Currently unused.
  ev_invalid_header <- read.table(
    "../data/ev_invalid_header.csv",
    header = T,
    colClasses = c("character", rep("double", 11), rep("character", 2)),
    sep = ",",
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = F,
    quote = "\"",
    fill = T,
    comment.char = "",
    dec = ".",
    check.names = F,
    strip.white = T,
  )
  
  # Description file with matching trait (column_labelling) entries with ev.
  suppressMessages(
    desc_ev_match <- read_csv(
      "../data/desc_ev_match.csv")
  )
})

# ----------------------
# Economic weights data
# ----------------------
suppressMessages({
  # Normal'good'data.Has just the required columns and values.
  ev_index_wt <- read.table(
    "../data/index_wt.csv",
    header = T,
    colClasses = c("character", "double"),
    sep = ",",
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = F,
    quote = "\"",
    fill = T,
    comment.char = "",
    dec = ".",
    check.names = F,
    strip.white = T,
  )
  
  # Indices are not unique.
  ev_index_dup_wt <- read.table(
    "../data/index_dup_wt.csv",
    header = T,
    colClasses = c("character", "double"),
    sep = ",",
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = F,
    quote = "\"",
    fill = T,
    comment.char = "",
    dec = ".",
    check.names = F,
    strip.white = T,
  )
  
  # Index name(s) does not exist in economic value file.
  ev_index_mismatch_wt <- read.table(
    "../data/index_mismatch_wt.csv",
    header = T,
    colClasses = c("character", "double"),
    sep = ",",
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = F,
    quote = "\"",
    fill = T,
    comment.char = "",
    dec = ".",
    check.names = F,
    strip.white = T,
  )
  
})


# --------------------------
# sanityCheckEBVdesc() tests
# --------------------------

# 1]
#' Check if the function does its job when the input file
#' just has the required  column_labelling and classifier columns.
test_that("sanityCheckEBVdesc() works for basic valid data", {
  txt <- sanityCheckEBVdesc(desc_bv)
  expect_true(is.null(txt)) # a NULL text means everythin's ok !
})

# 2]
#' Check if the function can parse data with optional columns.
#' when the input file only has the required  column_labelling and
#' classifier columns. Optional columns are unit,order and group.
test_that("sanityCheckEBVdesc() works for optional valid data", {
  txt <- sanityCheckEBVdesc(desc_bv_opt)
  expect_true(is.null(txt))
})

# 3]
#' Check if the function can detect a column header other than
#' column_labelling,classifier,unit,order and group.
test_that("sanityCheckEBVdesc() detects illegal header", {
  new_desc_ebv <- rename(desc_bv,
                         badClassifier = classifier)
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  expect_false(is.null(txt))
})

# 4]
#' Check if the function can return the correct error message
#' if a column header other than column_labelling,classifier,
#' unit,order,group is detected in the input file.
test_that("sanityCheckEBVdesc() reports illegal header", {
  new_desc_ebv <- rename(desc_bv,
                         badClassifier = classifier)
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  msg <-
    "Illegal header. Please use 'column_labelling' and 'classifier'.\n"
  expect_true(grepl(msg, txt))
})

# 5]
#' Check if the function can return the correct error message
#' if one of the required column headers (column_labelling and classifier)
#' are missing in the input file.
test_that("sanityCheckEBVdesc() detects missing header", {
  new_desc_ebv <- desc_bv
  names(new_desc_ebv) <- NULL
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  msg <-
    "Illegal header. Please use 'column_labelling' and 'classifier'.\n"
  expect_true(grepl(msg, txt))
})

# 6]
#' Check if the function can return the correct error message
#' if duplicate headers are detected in the input file.
test_that("sanityCheckEBVdesc() detects duplicate headers", {
  txt <- sanityCheckEBVdesc(dup_desc_bv)
  msg <- " Duplicated headers "
  expect_true(grepl(msg, txt))
})

# 7]
#' Check if the function can return the correct error message
#' if column_labelling  does not have a required unique ID field
#' in the input file.
test_that("sanityCheckEBVdesc() checks 'ID' exists", {
  no_ID_desc_ebv <- subset(desc_bv, classifier != "ID")
  txt <- sanityCheckEBVdesc(no_ID_desc_ebv)
  msg <- "At least one unique ID header has to be 'ID'"
  expect_true(grepl(msg, txt))
})

# 8]
#' Check if the function can return the correct error message
#' when one of the required classifiers are missing
#' in the input file. Required classifers are ID (for ID),
#' ClassVar (for categorical variables), EBV (for trait EBV) and
#' ACC (for trait accuracies).
test_that("sanityCheckEBVdesc() checks classifiers exist", {
  no_cls_desc_ebv <- subset(desc_bv, !classifier %in% c("EBV"))
  txt <- sanityCheckEBVdesc(no_cls_desc_ebv)
  msg <- " Missing classifier:\n"
  expect_true(grepl(msg, txt))
})

# 9]
#' Check if the function can return the correct error message
#' when one the classifiers are not one of ID (for ID),
#' ClassVar (for categorical variables), EBV (for trait EBV) and
#' ACC (for trait accuracies).
test_that("sanityCheckEBVdesc() checks for unknown classifiers", {
  new_desc_ebv <- desc_bv
  new_desc_ebv$classifier[new_desc_ebv$column_labelling == 'MOIST'] = 'GBV'
  txt <- sanityCheckEBVdesc(new_desc_ebv)
  msg <- " Unknown classifier:\n"
  expect_true(grepl(msg, txt))
})

# 10]
#' Check if the function can return the correct error message
#' when an ACC classifier does not have a corresponding 'TraitACC' entry
#' in the column_labelling colum of the input file.
test_that("sanityCheckEBVdesc() checks for Accuracy-Trait mismatch", {
  txt <- sanityCheckEBVdesc(test_desc_bv)
  msg <- " Accuracy names do not match trait names"
  expect_true(grepl(msg, txt))
})

# 11]
#' Check if the function can return the correct error message
#' when there are identical trait ordering values in the input file.
#' Trait ordering should be unique as they assign the order of traits
#' to show in tables and plots.
test_that("sanityCheckEBVdesc() checks for identical order numbers", {
  txt <- sanityCheckEBVdesc(desc_bv_identical_order)
  msg <- "there are identical order numbers"
  expect_true(grepl(msg, txt))
})

# 12]
#' Check if the function can return the correct error message
#' when there are missing ordering values for trait label/EBV rows
#' in the input file. While the order column is optional, every
#' trait label/EBV row should have an order value if the order
#' column is present.
test_that("sanityCheckEBVdesc() checks for missing order numbers", {
  txt <- sanityCheckEBVdesc(desc_bv_missing_order)
  msg <- "Missing order for trait(s)"
  expect_true(grepl(msg, txt))
})


# 13]
#' Check if the function can return the correct error message
#' when there are missing group assignment for traits in the input file.
#' While the group column is optional, every
#' trait label/EBV row should have a group value if the group
#' column is present in the input file.
test_that("sanityCheckEBVdesc() checks for missing group numbers", {
  txt <- sanityCheckEBVdesc(desc_bv_missing_group)
  msg <- "Missing group for trait(s)"
  expect_true(grepl(msg, txt))
})


# --------------------------
# sanityCheckEVdesc() tests
# --------------------------

# 1]
#' Check if the function does its job when the input file
#' just has the required  column_labelling and classifier columns.
test_that("sanityCheckEVdesc() works for basic valid data", {
  txt <- sanityCheckEVdesc(desc_ev, desc_bv)
  expect_true(is.null(txt)) # a NULL text means everythin's ok !
})

# 2]
#' Check if the function can detect a column header other than
#' column_labelling and classifier
test_that("sanityCheckEVdesc() detects illegal header", {
  bad_desc_ev <- rename(desc_ev,
                        badClassifier = classifier)
  txt <- sanityCheckEVdesc(bad_desc_ev, desc_bv)
  expect_false(is.null(txt))
})


# 3]
#' Check if the function can return the correct error message
#' if a column header other than column_labelling and classifier is
#' present in the input file.
test_that("sanityCheckEVdesc() reports illegal header", {
  bad_desc_ev <- rename(desc_ev,
                        badClassifier = classifier)
  txt <- sanityCheckEVdesc(bad_desc_ev, desc_bv)
  msg <-
    "Illegal header. Please use 'column_labelling' and 'classifier'.\n"
  expect_true(grepl(msg, txt))
})


# 4]
#'Check if the function can return the correct error message
#'if duplicate headers are detected in the input file.
test_that("sanityCheckEVdesc() reports duplicate headers", {
  dup_desc_ev <- desc_ev
  colnames(dup_desc_ev)[2] <- "column_labelling"
  txt <- sanityCheckEVdesc(dup_desc_ev, desc_bv)
  msg <- "Duplicated headers"
  expect_true(grepl(msg, txt))
})

# 5]
#' Check if the function can return the correct error message
#' if column_labelling  does not have a required unique
#' 'Index' ID header in the input file.
test_that("sanityCheckEVdesc() checks 'Index' exists", {
  no_index_desc_ev <- subset(desc_ev, column_labelling != "Index")
  txt <- sanityCheckEVdesc(no_index_desc_ev, desc_bv)
  msg <- "At least one unique ID header has to be 'Index'"
  expect_true(grepl(msg, txt))
})


# 6]
#' Check if the function can return the correct error message
#' when one of the required classifiers are missing
#' in the input file. Required classifers are ID (for ID),
#' and EV (for trait EV).
test_that("sanityCheckEVdesc() checks if required classifiers exist", {
  no_cls_desc_ev <- subset(desc_ev,!classifier %in% c("EV"))
  txt <- sanityCheckEVdesc(no_cls_desc_ev, desc_bv)
  msg <- "Missing classifier"
  expect_true(grepl(msg, txt))
})


# 7]
#' Check if the function can return the correct error message
#' when one the classifiers are not one of ID (for ID),
#' ClassVar (for categorical variables), EV (for trait EV) and
#' Group (group assignment for traits).
test_that("sanityCheckEVdesc() checks for unknown classifiers", {
  bad_cls_ev <- desc_ev
  bad_cls_ev$classifier[bad_cls_ev$column_labelling == 'TARSPOT'] = 'GEV'
  txt <- sanityCheckEVdesc(bad_cls_ev, desc_bv)
  msg <- "Unknown classifier"
  expect_true(grepl(msg, txt))
})


# 8]
#' Check if the function can return the correct error message
#' when the column_labelling contents in the input file are not
#' identical to those in the EBV description file.
test_that(
  "sanityCheckEVdesc() checks if trait(s) in the EV description file
   matches with those in the EBV description file",
  {
    txt <- sanityCheckEVdesc(desc_ev_mismatch_trait, desc_bv)
    msg <-
      "Trait(s) in EV description doesn't exist in EBV description:"
    expect_true(grepl(msg, txt, fixed = TRUE))
  }
)

# --------------------------
# sanityCheckEBV() tests
# --------------------------

# 1]
#' Check if the function does its job when the input file
#' just has the required columns in the correct format.
test_that("sanityCheckEBV() works for basic valid data", {
  txt <- sanityCheckEBV(bv, desc_bv)
  expect_true(is.null(txt)) # a NULL text means everythin's ok !
})


# 2]
#' Check if the function can return the correct error message
#' if duplicate headers are detected in the input file.
test_that("sanityCheckEBV() reports duplicate headers", {
  dup_bv <- bv
  colnames(dup_bv)[5] <- "ANTHRAC"
  txt <- sanityCheckEBV(dup_bv, desc_bv)
  msg <- "Duplicated headers"
  expect_true(grepl(msg, txt))
})


# 3]
#' Check if the function can return the correct error message
#' when the list of column headers in the input file is not
#' identical to that in the EBV description file.
test_that(
  "sanityCheckEBV() checks if the list of trait(s) in the
    EBV data file matches with that in the EBV description file",
  {
    tr_mismatch_bv <- bv[,!names(bv) %in% "TARSPOT"]
    txt <- sanityCheckEBV(tr_mismatch_bv, desc_bv)
    msg <-
      "Breeding value column header do not match file description"
    expect_true(grepl(msg, txt))
  }
)


# 4]
#' Check if the function can return the correct error messag
#' when column headers in the input file are not present
#' in the EBV description file.
test_that(
  "sanityCheckEBV() checks if traits in the EBV data file
    are present in the EBV description file",
  {
    no_desc_trait_bv <- bv
    no_desc_trait_bv$ANTHRAC_NZ = bv$ANTHRAC
    txt <- sanityCheckEBV(no_desc_trait_bv, desc_bv)
    msg <- "does not exist in description file"
    expect_true(grepl(msg, txt))
  }
)


# 5]
#' Check if the function can return the correct error message
#' when the input file contains character strings instead of numeric types.
test_that(
  "sanityCheckEBV() checks if traits values in the EBV data file
    are character strings instead of numbers",
  {
    char_trait_bv <- bv
    char_trait_bv[, 4] <- sapply(char_trait_bv[, 4], as.character)
    txt <- sanityCheckEBV(char_trait_bv, desc_bv)
    msg <- "Character strings detected"
    expect_true(grepl(msg, txt))
  }
)


# -------------------------
# sanityCheckEV() tests
# -------------------------

# 1]
#' Check if the function does its job when the input file just has
#' the required  columns in the correct format.
test_that("sanityCheckEV() works for basic valid data", {
  txt <- sanityCheckEV(ev, desc_ev_match)
  expect_true(is.null(txt)) # a NULL text means everythin's ok !
})


# 2]
#' Check if the function can return the correct error message when
#' the first column in the input file is not 'Index'.
test_that("sanityCheckEV() checks if first column is Index",
          {
            no_first_index_ev <- ev
            no_first_index_ev <-
              no_first_index_ev %>% relocate(Index, .after = TARSPOT)
            txt <- sanityCheckEV(no_first_index_ev, desc_ev_match)
            msg <- "First column name should be 'Index'"
            expect_true(grepl(msg, txt))
            
          })


# 3]
#' Check if the function can return the correct error message when
#' the 'Indices' are not unique in the input file.
test_that("sanityCheckEV() checks if 'Index' column is duplicated",
          {
            txt <- sanityCheckEV(ev_dup_index, desc_ev_match)
            msg <- "Duplicated indexes"
            expect_true(grepl(msg, txt))
            
          })

# 4]
#' Check if the function can return the correct error message when
#' a column header is duplicated in the input file.
test_that("sanityCheckEV() checks if a column header is duplicated",
          {
            txt <- sanityCheckEV(ev_dup_header, desc_ev_match)
            msg <- "Duplicated headers"
            expect_true(grepl(msg, txt))
            
          })


# 5]
#' Check if the function can return the correct error message when
#' the list of column headers in the input file does not match with
#' those in the description file.
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
#' Check if the function can return the correct error message when
#' column headers in the input file do not exist within
#' the description file.
test_that(
  "sanityCheckEV() checks if there is a column header in the input file
      that does not exist in the description file",
  {
    txt <- sanityCheckEV(ev_invalid_header, desc_ev_match)
    msg <-
      "does not exist in description file"
    expect_true(grepl(msg, txt))
    
  }
)

# -------------------------
# sanityCheckWt() tests
# -------------------------

# 1]
#' Check if the function does its job when the input file just has
#' the required  columns in the correct format.
test_that("sanityCheckWt() works for basic valid data", {
  txt <- sanityCheckWt(ev_index_wt, ev)
  expect_true(is.null(txt)) # a NULL text means everythin's ok !
})



# 2]
#' Check if the function can return the correct error message when
#' the 'Indices' are not unique in the input file.
test_that("sanityCheckWt() checks if 'Index' column is duplicated",
          {
            txt <- sanityCheckWt(ev_dup_index, ev)
            msg <- "Duplicated indexes"
            expect_true(grepl(msg, txt))
            
          })


# 3]
#' Check if the function can return the correct error message when
#' the 'Indices'in the input file are not recorded in the economic value file.
test_that("sanityCheckWt() checks if indices are not recorded in the economic
  value file",
          {
            txt <- sanityCheckWt(ev_index_mismatch_wt, ev)
            # Escape () to avoid being used to extract matched substrings in regexps.
            msg <-
              "Index name\\(s\\) does not exist in economic value files"
            expect_true(grepl(msg, txt))
            
          })


# 4]
#' Check if the function can return the correct error message when
#' character strings are detected in the 'weight' column of the input file.
test_that(
  "sanityCheckWt() checks if character strings are detected in
  the 'weight' column of the index weight file",
  {
    ev_char_wt <- ev_index_wt
    ev_char_wt[, 2] <- sapply(ev_char_wt[, 2], as.character)
    txt <- sanityCheckWt(ev_char_wt, ev)
    msg <- "Character strings detected"
    expect_true(grepl(msg, txt))
    
  }
)


# 5]
#' Check if the function can return the correct error message when
#' NA's are detected in the 'weight' column of the input file.
test_that(
  "sanityCheckWt() checks if NA's are detected in
  the 'weight' column of the index weight file",
  {
    ev_char_wt <- ev_index_wt
    ev_char_wt[3, 2] <- NA
    txt <- sanityCheckWt(ev_char_wt, ev)
    msg <- "NA detected"
    expect_true(grepl(msg, txt))
    
  }
)
