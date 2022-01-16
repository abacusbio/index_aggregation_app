#' Setup.R
#'
#' Test harness initialization.
#' @author Sameer Atre <SAtre@@abacusbio.co.nz>


#' source("setup.R")


# Everything here is executed before any test is run.
# We want a clean test report with less clutter.
suppressWarnings({
  suppressPackageStartupMessages({
    library(testthat)
    library(readxl)
    library(tidyverse)
    library(shiny)
  })
})


