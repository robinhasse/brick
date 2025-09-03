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
#' @importFrom reportbrick showSankey
#' @export
#'
startModel <- function(path, runReporting = TRUE) {

  cfg <- readConfig(file.path(path, "config", "config_COMPILED.yaml"), readDirect = TRUE)

  if (file.exists(file.path(path, "config", "restartOptions.csv"))) {
    restart <- read.csv2(file.path(path, "config", "restartOptions.csv"))[["restart"]]
  } else {
    restart <- FALSE
  }

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

  checkGamsSuccess(path)

  plotSummary(path, NULL)

  # sankey plots
  try(showSankey(path, "hs", maxPeriodsInRow = 5))
  if (!isTRUE(cfg[["ignoreShell"]])) {
    try(showSankey(path, "bs", maxPeriodsInRow = 5))
  }

  if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
    plotRefDeviation(path)
    plotSummary(path, c("loc", "typ"))
  }

  if (isTRUE(runReporting)) {
    try(reportMif(path))
  }

}
