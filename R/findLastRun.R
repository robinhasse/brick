#' Determine the latest path of a run
#'
#' Search the given output folder for the run with the most recent time stamp
#' and return the path to this run.
#' If the output folder contains only one run, return its path, also if it does not contain a time stamp.
#'
#' @author Ricarda Rosemann
#'
#' @param outputFolder character, output folder to search in
#'
findLastRun <- function(outputFolder) {

  if (dir.exists(outputFolder)) {
    paths <- list.dirs(outputFolder, recursive = FALSE)
    regexStamp <- "\\d{4}-\\d{2}-\\d{2}_\\d{2}\\.\\d{2}\\.\\d{2}"
    stamp <- sub(
      paste0("^.*_(", regexStamp, ")$"),
      "\\1",
      grep(regexStamp, paths, value = TRUE)
    )
    if (length(stamp) > 0) {
      path <- paths[which(stamp == max(stamp))]
    } else if (length(paths) == 1) {
      path <- paths
    } else {
      stop("Cannot identify the most recent run. Please provide a path")
    }

  } else {
    stop("Cannot find last run as this outputFolder does not exist: ",
         outputFolder)
  }

  return(path)
}
