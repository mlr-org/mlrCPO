context("lint")
if (isLintrVersionOk(identical(Sys.getenv("TRAVIS"), "true"))) {
  test_that("lint check", {
    library("lintr")
    library("rex")
    # linters are defined in help_lint.R
    if (identical(Sys.getenv("R_COVR"), "true")) {
      skip("Coverage")
    }
    expect_lint_free(linters = linters)
#      exclusions = paste0("inst/doc/", list.files("../../inst/doc/", ".*\\.R$")))
  })
} else {
  warning(paste("lintr test was disabled because of missing lintr.",
    "To run lintr test, please install the github version of lintr by running",
    "> devtools::install_github(\"jimhester/lintr\")", sep = "\n"))
}

