test_that(".gms scripts in inst/gams/matching are independent", {

  folder <- piamutils::getSystemFile("gams", "matching", package = "brick")
  scripts <- list.files(folder, pattern = "\\.gms$")

  for (script in scripts) {
    file <- file.path(folder, script)
    ref <- sub("^(.+)\\.gms$", "\\1", script)
    code <- readLines(file, warn = FALSE)

    sameasPattern <- paste0("\\$sameas\\(ref,\"", ref, "\"\\)")
    refMapPattern <- paste0(.refMapName(ref), "\\(")
    codeWithoutRefMap <- gsub(refMapPattern, "", code)

    expect_match(code, sameasPattern, all = FALSE, ignore.case = TRUE)
    expect_no_match(codeWithoutRefMap, .refMapName(""), ignore.case = TRUE)
  }
})
