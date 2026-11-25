#' Load calibration targets from matching results
#'
#' @author Ricarda Rosemann
#'
#' @param config named list with run configuration
#' @param path character vector with folders to write input data into
#'
loadCalibrationTarget <- function(config, path) {

  defaultTargetFolder <- file.path(madrat::getConfig("sourcefolder"), "BrickMatching")

  # read path with calibration targets from switch
  if (is.null(config$matchingRun) || !is.character(config$matchingRun)) {
    targetPath <- NULL
  } else if (dir.exists(config$matchingRun)) {
    targetPath <- config$matchingRun
  } else {
    targetPath <- file.path(defaultTargetFolder, config$matchingRun)
    if (dir.exists(targetPath)) {
      message("You did not specify a full path as 'matchingRun'. ",
              "Calibration targets are read from ", targetPath, ".")
    } else {
      warning("You did not specify an existing full path as 'matchingRun' ",
              "and 'matchingRun' is not a subdirectory of the default matching folder\n",
              defaultTargetFolder, ".")
      targetPath <- NULL
    }
  }

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
      filePath <- file.path(path, paste0("f_", v, "CalibTarget.cs4r"))
      file.exists(filePath)
    })
    if (all(targetFilesExist)) {
      message("No valid matching path specified, but all required calibration target files are",
              "already in the run folder\n", path, ".\n",
              "No files are copied, the calibration is executed based on the existing files.")
      return(invisible(NULL))
    } else {
      stop("No valid matching path specified and at least one calibration target file ",
           "is missing in the run folder\n", path, ".")
    }
  }

  # copy the calibration target files to the input directory
  lapply(vars, function(v) {
    fileName <- paste0("f_", v, "CalibTarget.cs4r")
    filePath <- file.path(targetPath, fileName)
    if (file.exists(filePath)) {
      file.copy(filePath, path, overwrite = TRUE)
    } else {
      stop("The file ", fileName, " does not exist in the directory ", targetPath, ".")
    }
  })
  message("Copied all calibration target files from ", targetPath, " to ", path, ".")

  return(invisible(NULL))

}
