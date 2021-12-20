# Purpose : Test data for the sanity check functions in
#           function_preprocess.R
# Author  : Sameer Atre
#           SAtre@abacusbio.co.nz

# Everything here is executed before any test is run.
# For data formats and required fields see the index testing app at
# https://abacusbio.shinyapps.io/selection_index_revamp/

#  We want a clean test report with less clutter.
suppressWarnings({
  suppressPackageStartupMessages({
    library(testthat)
    library(readxl)
    library(tidyverse)
  })
})

# ----------------------------------
# Breeding value file descriptions
# ----------------------------------

# Normal'good'data.Has just the required columns and values.
desc_bv <- read_xlsx(
  "../data/description_bv.xlsx",
  sheet = 1,
  skip = 0,
  col_types = rep("text", 2),
  col_names = T,
  trim_ws = T
)

# Normal'good'data but with optional fields.
suppressMessages(desc_bv_opt <-
                   read_csv("../data/description_bv_optional.csv"))


# Has duplicate column headers.
dup_desc_bv <- read_xlsx(
  "../data/description_bv_duplicate_hdr.xlsx",
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
    "../data/description_bv_acc_trait_mismatch.xlsx",
    sheet = 1,
    skip = 0,
    col_types = rep("text", 2),
    col_names = T,
    trim_ws = T
  )

# Has identical order values in trait label/EBV rows.
suppressMessages(desc_bv_identical_odr <- read_csv(
  "../data/description_bv_identical_order.csv")
  )

# Has missing order values in trait label/EBV rows.
suppressMessages(desc_bv_missing_odr <- read_csv(
  "../data/description_bv_missing_order.csv")
  )

# Has missing group values in trait label/EBV rows.
suppressMessages(desc_bv_missing_grp <- read_csv(
  "../data/description_bv_missing_group.csv")
  )


# ----------------------------------
# Economic value file descriptions
# ----------------------------------

# Normal'good'data.Has just the required columns and values.
suppressMessages(desc_ev <- read_csv(
  "../data/description_ev.csv")
  )

# Column_labelling contents are not identical to those in the 
# EBV description file.
suppressMessages(desc_ev_tr_mismatch <- read_csv(
  "../data/description_ev_trait_mismatch.csv")
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
  ev_dup_hdr <- read.table(
    "../data/ev_dup_hdr.csv",
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
  
  ev_mismatch_hdr <- read.table(
    "../data/ev_mismatch_hdr.csv",
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
  
  ev_invalid_hdr <- read.table(
    "../data/ev_invalid_hdr.csv",
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
  
  suppressMessages(
    desc_ev_match <- read_csv(
      "../data/description_ev_match.csv")
    )
})

# ----------------------
# Economic weights data
# ----------------------
suppressMessages({
  # Normal'good'data.Has just the required columns and values.
  ev_wt_index <- read.table(
    "../data/index_weight.csv",
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
  ev_wt_dup_index <- read.table(
    "../data/index_weight_dup.csv",
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
  ev_wt_mismatch_index <- read.table(
    "../data/index_weight_mismatch.csv",
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
