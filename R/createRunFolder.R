#' Create new run folder
#'
#' Create a folder for the model to run in and copy required gams files there.
#'
#' Create either one or multiple
#'
#' @author Robin Hasse
#'
#' @param path character vector, containing
#' @param config list with run configuration
#' @param overwrite logical; Should exiting folders be overwritten?
#' @param recursive logical; Should exiting folders be overwritten?
#' @param showWarnings logical; Should exiting folders be overwritten?
#'
#' @importFrom yaml write_yaml
#' @export
#'
createRunFolder <- function(path,
                            config = NULL,
                            overwrite = FALSE,
                            recursive = FALSE,
                            showWarnings = TRUE) {

  # find files -----------------------------------------------------------------

  # recognised file names for inital gdx (in order of priority)
  initialGdxNames <- c("output.gdx")

  # find file for starting point
  initialGdx <- config[["startingPoint"]]
  if (length(initialGdx) > 1) {
    stop("Don't give more than one starting point. You gave:\n",
         paste(initialGdx, collapse = "\n"))
  }
  initialGdxFiles <- file.path(initialGdx, initialGdxNames)
  initialGdxFile <- if (!is.null(initialGdx)) {
    if (file.exists(initialGdx)) {
      initialGdx
    } else if (dir.exists(initialGdx)) {
      if (any(file.exists(initialGdxFiles))) {
        head(initialGdxFiles[which(file.exists(initialGdxFiles))], 1)
      } else {
        stop("There is no file for the starting point in the given directory: ",
             initialGdx)
      }
    } else {
      stop("Unable to find a file for the starting point with given config ",
           "parameter: ", initialGdx)
    }
  } else {
    NULL
  }



  # create new run folders -----------------------------------------------------

  newPaths <- unique(path[!dir.exists(path) | overwrite])
  missingPaths <- setdiff(path, newPaths)
  dir.create(newPaths, recursive = recursive)

  # warnings for paths that could not be created
  if (showWarnings && length(missingPaths > 0)) {
    warning(length(missingPaths), " out of ", length(path), " paths have not ",
            "been created:\n  ",
            paste(missingPaths, collapse = "\n  "))
  }



  # copy files -----------------------------------------------------------------

  ## gams files ====
  gamsFiles <- file.path("inst", "gams", ".")
  file.copy(gamsFiles, newPaths, recursive = TRUE, overwrite = overwrite)


  ## starting point ====
  if (!is.null(initialGdxFile)) {
    file.copy(initialGdxFile, file.path(newPaths, "start.gdx"),
              overwrite = overwrite)
    cat("Using", initialGdxFile, "as start.gdx")
  }


  ## config ====
  configFile <- attr(config, "file", exact = TRUE)
  configFolder <- file.path(newPaths, "config")
  if (!dir.exists(configFolder)) {
    dir.create(configFolder)
  }
  if (file.exists(configFile)) {
    file.copy(configFile, configFolder, overwrite = overwrite)
  }
  write_yaml(config, file.path(configFolder, "config.yaml"))

}
