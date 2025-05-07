findSameas <- function(code, ref) {
  pattern <- paste0("\\$sameas\\(ref,\"", ref, "\"\\)")
  any(grepl(pattern, code, ignore.case = TRUE))
}


usesAnotherRefMap <- function(code, ref) {
  removePattern <- paste0(.refMapName(ref), "\\(")
  codeWithoutRefMap <- gsub(removePattern, "", code)
  any(grepl(.refMapName(""), codeWithoutRefMap, ignore.case = TRUE))
}


test_that("All .gms scripts in inst/gams/matching comply with rules", {

  folder <- piamutils::getSystemFile("gams", "matching", package = "brick")
  scripts <- list.files(folder, pattern = "\\.gms$")

  for (script in scripts) {
    file <- file.path(folder, script)
    ref <- sub("^(.+)\\.gms$", "\\1", script)
    code <- readLines(file, warn = FALSE)

    expect_true(
      findSameas(code, ref),
      info = paste0("Script ", file, " must contain $sameas(ref,\"", ref, "\")")
    )

    expect_false(
      usesAnotherRefMap(code, ref),
      info = paste("Script", file, "uses a refMap from another reference.")
    )
  }
})
