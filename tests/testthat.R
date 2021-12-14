library(testthat)
test_dir(
  "tests/testthat",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)

