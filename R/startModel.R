#' Start the model
#'
#' Run the model with given configuration.
#'
#' This function creates a run folder with necessary gams files if missing. It
#' then computes the input data and finally runs the optimisation.
#'
#' @author Robin Hasse
#'
#' @param path character vector with folders to run the model in
#' @param runReporting logical, whether to run the reporting, i.e. write the mif
#'
#' @importFrom utils read.csv2
#' @importFrom reportbrick showSankey plotMatchingComparison
#' @export
#'
startModel <- function(path, runReporting = TRUE) {

  cfg <- readConfig(file.path(path, "config", CONFIG_COMPILED), readDirect = TRUE)

  restart <- .readInitArgs(path)$restart

  if (isFALSE(restart) || "createInput" %in% restart) {
    createInputData(path, cfg, overwrite = !isFALSE(restart))
  }

  if (cfg[["switches"]][["RUNTYPE"]] == "matching" &&
        (isFALSE(restart) || "createMatching" %in% restart)) {
    createMatchingData(path, cfg, overwrite = !isFALSE(restart))
  }

  if (isFALSE(restart) || any(c("createMatching", "copyGams") %in% restart)) {
    if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
      insertMatchingCode(path)
    } else if (cfg[["switches"]][["RUNTYPE"]] == "calibration") {
      # ????
    }
  }

  if ("reweightMatching" %in% restart) {
    reweightMatchingReferences(path)
  }


  if (cfg[["switches"]][["SOLVEPROBLEM"]] == "auto") {
    cfg[["switches"]][["SOLVEPROBLEM"]]  <- if (is.null(cfg[["startingPoint"]])) {
      "lpnlp"
    } else {
      "nlp"
    }
  }

  if (cfg[["switches"]][["RUNTYPE"]] == "calibration") {
    runCalibration(path,
                   parameters = cfg[["calibrationParameters"]],
                   tcalib = cfg[["calibperiods"]],
                   gamsOptions = cfg[["gamsOptions"]],
                   switches = c(cfg[["switches"]],
                                cfg[c("solverLP", "solverNLP", "solverQCP", "ignoreShell")]),
                   gamsCall = cfg[["gamsCall"]])
  } else {
    runGams(path,
            cfg[["gamsOptions"]],
            c(cfg[["switches"]], cfg[c("solverLP", "solverNLP", "solverQCP",
                                       "ignoreShell")]),
            gamsCall = cfg[["gamsCall"]])
  }

  checkGamsSuccess(path, isCalibration = cfg[["switches"]][["RUNTYPE"]] == "calibration")



  if (isTRUE(runReporting)) {
    plotSummary(path, NULL)

    # sankey plots
    try(showSankey(path, "hs", maxPeriodsInRow = 5))
    if (!isTRUE(cfg[["ignoreShell"]])) {
      try(showSankey(path, "bs", maxPeriodsInRow = 5))
    }

    if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
      plotRefDeviation(path)
      plotMatchingComparison(path)
      plotSummary(path, c("loc", "typ"))
    }

    try(reportMif(path))
  }

  # if (isFALSE(restart)) {
    .startNextScens(path)
  # }

}



#' Read model initialisation arguments
#'
#' Since its function call, \code{initModel} might have changed arguments,
#' namely the restart options.
#'
#' @param path character, path to run folder
#' @returns named list of arguments to \code{initModel}
.readInitArgs <- function(path) {
  yaml::read_yaml(file.path(path, "config", "init.args"))
}


.startNextScens <- function(path) {
  pathNextConfigs <- file.path(path, "config", "nextRuns")
  if (dir.exists(pathNextConfigs)) {
    nextConfigs <- readNextConfigs(pathNextConfigs)
    args <- .readInitArgs(path)
    args$config <- nextConfigs
    args$path <- NULL
    do.call(initModel, args)
  }
}
