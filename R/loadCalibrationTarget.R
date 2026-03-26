#' Load calibration targets from matching results
#'
#' @author Ricarda Rosemann
#'
#' @param config named list with run configuration
#' @param inputDir directory of input folder
#' @param path character, path to the run
#'
loadCalibrationTarget <- function(config, inputDir, path) {

  .makeFileName <- function(x) {
    paste0("f_", x, "CalibTarget.cs4r")
  }

  # read path with calibration targets from switch
  targetPath <- .normaliseMatchingPath(config[["matchingRun"]])

  # set variables names of calibration targets
  sequentialRen <- isTRUE(config$switches$SEQUENTIALREN)
  vars <- c(
    stock = "stock",
    construction = "construction",
    demolition = "demolition"
  )

  if (sequentialRen) {
    vars[["renovationHS"]] <- "renovationHS"
    vars[["renovationBS"]] <- "renovationBS"
  } else {
    vars[["renovation"]] <- "renovation"
  }

  # Do not copy files if matchingRun switch is NULL or specified path/folder does not exist
  if (is.null(targetPath)) {
    targetFilesExist <- lapply(vars, function(v) {
      filePath <- file.path(inputDir, .makeFileName(v))
      file.exists(filePath)
    })
    if (all(targetFilesExist)) {
      message("No valid matching path specified, but all required calibration target files are",
              "already in the input directory\n", inputDir, ".\n",
              "No files are copied, the calibration is executed based on the existing files.")
      return(invisible(NULL))
    } else {
      stop("No valid matching path specified and at least one calibration target file ",
           "is missing in the input directory\n", inputDir, ".")
    }
  }

  # copy the calibration target files to the input directory
  lapply(vars, function(v) {
    fileName <- .makeFileName(v)
    filePath <- file.path(targetPath, fileName)
    if (file.exists(filePath)) {
      file.copy(filePath, inputDir, overwrite = TRUE)
    } else {
      stop("The file ", fileName, " does not exist in the directory ", targetPath, ".")
    }
  })
  message("Copied all calibration target files from ", targetPath, " to ", inputDir, ".")

  return(invisible(NULL))

}
