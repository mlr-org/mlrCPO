library(testthat)
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("mlrCPO", filter = "^_core_")
}

