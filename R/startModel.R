#' Start the model
#'
#' Run the model with given configuration.
#'
#' This function creates a run folder with necessary gams files if missing. It
#' then computes the input data and finally runs the optimisation.
#'
#' @author Robin Hasse
#'
#' @param config run configurations
#' @param path character vector with folders to run the model in
#' @param outputFolder directory of output folder
#' @export
#'
startModel <- function(config = NULL,
                       path = NULL,
                       outputFolder = "output") {

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  cfg <- readConfig(config)
  title <- cfg[["title"]]

  if (is.null(path)) {
    stamp <- format(Sys.time(), "_%Y-%m-%d_%H.%M.%S")
    path <- file.path(outputFolder, paste0(title, stamp))
  }

  createRunFolder(path, cfg)

  createInputData(path, cfg)

  runGams(path,
          cfg[["gamsOptions"]],
          c(cfg[["switches"]], cfg[c("solverLP", "solverNLP")]),
          gamsCall = cfg[["gamsCall"]])

  plotSummary(path)
}
