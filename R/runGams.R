#' Run gams optimisation
#'
#' Run the gams model in a given directory.
#'
#' The file `input.gdx` has to exist in the given directory.
#'
#' @author Robin Hasse
#'
#' @param path character vector with folders to run gams in
#' @param flags list of gams flags
#' @param fileName character vector with gams file names
#' @param gamsCall system command to call gams
#'
#' @export
#'
runGams <- function(path,
                    gamsOptions = NULL,
                    switches = NULL,
                    fileName = "main.gms",
                    gamsCall = "gams") {

  # check length of parameters
  if (!length(fileName) %in% c(1, length(path))) {
    stop("'fileName' has to have a length of ", length(path),
         ifelse(length(path) > 1, " or 1", ""), ", not ", length(fileName))
  }
  if (!length(gamsCall) %in% c(1, length(path))) {
    stop("'gamsCall' has to have a length of ", length(path),
         ifelse(length(path) > 1, " or 1", ""), ", not ", length(gamsCall))
  }


  # call this function for each path
  if (length(path) > 1) {
    return(mapply(runGams, path, fileName, gamsCall))
  }

  # standard gams options
  if (is.null(gamsOptions)) {
    gamsOptions <- list()
  }
  if (!"logoption" %in% tolower(names(gamsOptions))) {
    gamsOptions[["logoption"]] <- 2  # Log output to logfile
  }

  # helpers
  flagString <- paste(toolMakeHandle(gamsOptions),
                      toolMakeHandle(switches, "model"))

  # check if input.gdx exists
  if (!file.exists(file.path(path, "input.gdx"))) {
    stop("There is no 'input.gdx' in ", path)
  }

  # run gams
  wd <- getwd()
  setwd(path)

  system(paste(gamsCall, fileName, flagString))

  setwd(wd)

  cat("GAMS started:", file.path(path, fileName))
}
