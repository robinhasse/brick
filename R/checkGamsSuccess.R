#' Check whether Gams finished successfully
#'
#' Check which output file was written and derive state of Gams run
#'
#' If \code{output.gdx} was written, give success message.
#' If \code{abort.gdx} was written or no output file exists, stop with error message.
#' For calibration runs:
#' If any file with \code{calibration_<iteration>.gdx} was written, give success message
#' including the number of iterations.
#' If no file was written, stop with error message.
#'
#' @author Ricarda Rosemann
#'
#' @param path character, path to search for gams output files
#' @param isCalibration logical, whether this is a calibration run
#'
checkGamsSuccess <- function(path, isCalibration = FALSE) {
  if (isTRUE(isCalibration)) {
    allCalibrationFiles <- list.files(path, pattern = "calibration_\\d+\\.gdx$")
    if (length(allCalibrationFiles) > 0) {
      maxIteration <- max(as.numeric(sub("calibration_(\\d+)\\.gdx", "\\1", allCalibrationFiles)))
      message("Gams successfully completed ", maxIteration, " calibration iterations: ",
              "\"calibration_", maxIteration, ".gdx\" exists.")
    } else {
      stop("Gams did not finish successfully.\n",
           "Check the files main.log and main.lst for more information.")
    }
  } else {
    if (file.exists(file.path(path, "output.gdx"))) {
      message("Gams was successful: \"output.gdx\" exists.")
    } else if (file.exists(file.path(path, "abort.gdx"))) {
      stop("Gams aborted with \"abort.gdx\" due to errors in at least one variable.")
    } else {
      stop("Gams did not finish successfully.\n",
           "Check the files main.log and main.lst for more information.")
    }
  }
}
