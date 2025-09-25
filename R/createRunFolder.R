#' Create new run folder
#'
#' Create a folder for the model to run in and copy required gams files there.
#'
#' @param path character vector, containing the path(s) to the folder(s) to be created.
#' @param config list with run configuration
#' @param overwrite logical, should existing folders be overwritten?
#' @param recursive logical, should elements of the path other than the last be created?
#' @param showWarnings logical, should a warning on not created paths be shown?
#'
#' @author Robin Hasse
#'
#' @importFrom yaml write_yaml
#' @importFrom reportbrick readBrickSets

createRunFolder <- function(path,
                            config = NULL,
                            overwrite = FALSE,
                            recursive = FALSE,
                            showWarnings = TRUE) {

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




  ## config ====

  configFile <- attr(config, "file", exact = TRUE)
  configFolder <- file.path(newPaths, "config")
  if (!dir.exists(configFolder)) {
    dir.create(configFolder)
  }
  if (file.exists(configFile)) {
    file.copy(configFile, configFolder, overwrite = overwrite)
  }
  write_yaml(config, file.path(configFolder, "config_COMPILED.yaml"))


  ## reporting template ====

  brickSets <- readBrickSets(config[["reportingTemplate"]])
  write_yaml(brickSets, file.path(configFolder, "brickSets_COMPILED.yaml"))

}
