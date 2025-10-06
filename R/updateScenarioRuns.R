#' Update bundle of scenario runs
#'
#' Run a bundle of scenario runs or update selected runs. This function requires
#' local configs and a bundle setting file. The settings file serves two
#' purposes: First, It holds the settings that control which scenarios should be
#' updated when running this function. Second, it holds the run paths to the
#' most recent runs that should be used by post processing, reporting or
#' plotting routines.
#'
#' This function provides convenient features for scenario studies but it
#' assumes a specific setup that is advised to follow and might not permanent
#' otherwise:
#' \itemize{
#'   \item Choose one central directory from where you call this function.
#'   \item Place a bundle settings file in this directory. You can use this
#'     function to create a template.
#'   \item Place a config folder and an output folder inside the central
#'     directory and reference both as relative paths in the settings file.
#' }
#' Mind that this function changes the config files. This should only affect the
#' starting point. But it is advisable to commit the config files to git before
#' to avoid loosing information.
#'
#' @author Robin Hasse
#'
#' @param bundleSettings character, path to settings file. If the file doesn't
#'   exist, a new file is written as a template.
#' @param startFromRun logical, if TRUE, start from previous run saved in
#'   settings.yaml
#' @returns path to the started run

updateScenarioRuns <- function(bundleSettings, startFromRun = TRUE) {

  if (!identical(normalizePath(getwd()),
                 normalizePath(dirname(bundleSettings)))) {
    warning("Working directory and settings file location are not identical. ",
            "This can lead to unexpected behaviour.")
  }

  if (!file.exists(bundleSettings)) {
    .createNewSettings(bundleSettings)
    message("Bundle settings file did not exist so a new template was written: ",
            bundleSettings,
            "\nAdapt the template and rerun.")
    return(invisible(NULL))
  }

  settings <- .readSettings(bundleSettings)
  configs <- .getConfigs(settings)

  runPaths <- c()

  for (scen in names(configs)) {

    runPath <- settings[["run"]][[scen]]
    config <- configs[[scen]]

    message("\n\n##### ", toupper(scen), " #####\n")

    # check if BRICK has to be started or an old run can be used instead -------

    if (isTRUE(settings[["newRunAlways"]])) {
      settings[["newRunNext"]][[scen]] <- TRUE
    } else {
      settings[["newRunAlways"]] <- FALSE
      if (isFALSE(settings[["newRunNext"]][[scen]])) {
        if (.fileExists(settings[["run"]][[scen]])) {
          message("BRICK is not started and an old run is used for ",
                  scen, " instead. ",
                  "Change settings file if yo want to start BRICK.")
          yaml::write_yaml(settings, bundleSettings)
          runPaths[[scen]] <- runPath
          next
        } else {
          message("The run in settings file doesn't exist.",
                  "BRICK has to be started.")
        }
      } else {
        settings[["newRunNext"]][[scen]] <- TRUE
      }
    }



    # previous run as starting point -------------------------------------------

    startGdx <- file.path(settings[["run"]][[scen]], "output.gdx")

    if (!is.null(settings[["run"]][[scen]]) &&
          isTRUE(startFromRun) &&
          .fileExists(startGdx)) {

      .changeConfig(file.path(settings$configFolder, config),
                    param = "startingPoint",
                    value = settings[["run"]][[scen]],
                    comment = "previous run from settings.yaml")

    }



    # last base run as historic ------------------------------------------------

    if (scen != "base") {
      historyGdx <- file.path(runPaths[["base"]], "output.gdx")

      if (.fileExists(historyGdx)) {

        .changeConfig(file.path("config", config),
                      param = "historic",
                      value = historyGdx,
                      comment = "previous base run from settings.yaml")

      }
    }



    # start BRICK --------------------------------------------------------------

    runPath <- brick::initModel(config = config,
                                configFolder = "config",
                                outputFolder = "runs",
                                sendToSlurm = settings[["sendToSlurm"]])



    # update settings.yaml and return run path ---------------------------------

    if (.fileExists(file.path(runPath, "output.gdx"))) {
      message("The run seems successful and is saved in settings.yaml.")
      settings[["run"]][[scen]] <- runPath
      if (!settings[["newRunAlways"]]) {
        settings[["newRunNext"]][[scen]] <- FALSE
      }
      yaml::write_yaml(settings, bundleSettings)
      runPaths[[scen]] <- runPath
      next
    }

    stop("Cannot find output.gdx in the run: ", runPath)
  }



  return(invisible(runPaths))

}


#' Read bundle settings
#'
#' This function also checks that the settings contain all required keys and
#' only arguments to \code{initModel} as further arguments.
#'
#' @param path character, path to bundle settings file
#' @returns named list with settings

.readSettings <- function(path) {
  settings <- yaml::read_yaml(path)
  keys <- names(settings)

  requiredKeys <- c("newRunAlways", "run", "newRunNext", "configFolder")
  missingKeys <- setdiff(requiredKeys, keys)
  if (length(missingKeys) > 0) {
    stop("Missing keys in settings file ", path, " detected: ",
         paste(missingKeys, collapse = ", "))
  }

  furtherAllowedKeys <- setdiff(names(formals(initModel)), c("config", "path", "restart"))
  wrongKeys <- setdiff(keys, union(requiredKeys, furtherAllowedKeys))
  if (length(wrongKeys) > 0) {
    stop("Unallowed keys in settings file ", path, " detected: ",
         paste(wrongKeys, collapse = ", "))
  }

  attr(settings, "dirname") <- dirname(path)

  return(settings)
}



#' Change config switch
#'
#' Reset value of specified switch in given config. If the switch has not been
#' found in the config before, it is appended at the end to avoid disturbing the
#' nesting of the yaml file.
#'
#' @param configPath character, file path to config
#' @param param character, name of switch
#' @param value value of switch
#' @param comment character, comment behind switch value

.changeConfig <- function(configPath, param, value, comment = NULL) {
  cfg <- readLines(configPath)
  pattern <- paste0("^", param, ":")
  pos <- grep(pattern, cfg)
  cfg <- cfg[!grepl(pattern, cfg)]
  newLine <- paste0(param, ": \"", value, "\"")
  if (!is.null(comment)) {
    newLine <- paste(newLine, "#", comment)
  }
  cfg <- append(cfg, newLine, after = if (length(pos) == 1) {pos - 1} else {length(cfg)})
  writeLines(cfg, configPath)
}



#' Get configs
#'
#' @param settings named list with bundle settings
#' @returns named list of selected config files

.getConfigs <- function(settings) {
  wd <- getwd()
  setwd(attr(settings, "dirname"))
  configFolder <- settings$configFolder
  if (!dir.exists(configFolder)) {
    setwd(wd)
    stop("Config folder doesn't exist: ", configFolder)
  }
  configs <- list.files(configFolder, "^.+\\.yaml$")
  setwd(wd)
  configNames <- .getFileName(configs)
  selectedConfigs <- names(settings$run)
  missingConfigs <- setdiff(selectedConfigs, configNames)
  if (length(missingConfigs) > 0) {
    stop("Can't find the following configs in the config folder: ",
         paste(missingConfigs, collapse = ", "),
         if (all(missingConfigs %in% .getPlaceholderConfigNames())) {
           "\nYou might need to specify placeholder config names in the settings."
         } else {
           ""
         })
  }
  setNames(configs, configNames)[selectedConfigs]
}



#' Get placeholder config names
#'
#' @returns vector of config names

.getPlaceholderConfigNames <- function() {
  paste0("<config", 1:3, ">")
}



#' Create new scenario bundle settings file
#'
#' Writes template for settings file to given path. The file might have to be
#' revisited to specify place holders.
#'
#' @param path character, path of file to be written
#' @param configFolderName character, config folder to look for next to settings
#'   file to be written. It is advised to have the config folder and the
#'   settings file in the same directory but any sub directory relative to the
#'   settings file is accepted.
#' @returns template (invisibly)
#'
#' @author Robin Hasse

.createNewSettings <- function(path, configFolderName = "config") {

  configFolder <- file.path(dirname(path), configFolderName)
  configFolderRel <- if (dir.exists(configFolder)) {
    configFolder
  } else {
    warning("No config folder found. ",
            "Please revisit config folder path in the template.")
    paste0("<", configFolderName, ">")
  }

  configs <- sub("\\..*$", "", list.files(configFolder, "\\.yaml$"))
  if (length(configs) == 0) {
    configs <- .getPlaceholderConfigNames()
    warning("No potential config files found. ",
            "Please revisit run names in the template.")
  }

  template <- c(
    "newRunAlways: no",
    "run:",
    paste0("  ", configs, ": ~"),
    "newRunNext:",
    paste0("  ", configs, ": no"),
    paste0("configFolder: ", configFolderRel)
  )

  writeLines(template, path)
  return(invisible(template))
}
