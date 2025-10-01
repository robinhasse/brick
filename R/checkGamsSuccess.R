#' Check whether Gams finished successfully
#'
#' Check which file is the gdx file written by Gams from main.log.
#' Read model and solver status from this gdx file and print.
#'
#' Stop with error message if:
#'   - \code{main.log} does not exist
#'   - the gdx file does not exist
#'   - more than one gdx file was written.
#'
#' @author Ricarda Rosemann
#'
#' @param path character, path to search for gams output files
#' @param silent logical, whether no messages should be printed.
#'
#' @returns logical, whether GAMS was successful for all subsets
#'
#' @importFrom dplyr %>% .data mutate
#' @importFrom tidyr pivot_wider
#'
checkGamsSuccess <- function(path, silent = FALSE) {

  .extractGdxPath <- function(lines, pattern) {
    sub(paste0(".*", pattern), "", lines)
  }

  .checkModelWasSuccessful <- function(p_repy, desiredSolverStatus = 1, desiredModelStatus = c(1, 2)) {
    if (nrow(p_repy) > 0) {
      p_repy %>%
        pivot_wider(names_from = "solveinfo") %>%
        mutate(success = .data$solvestat %in% desiredSolverStatus & .data$modelstat %in% desiredModelStatus) %>%
        select(-any_of(p_repy$solveinfo))
    } else {
      NULL
    }
  }

  # Check for gdx written in latest main.log
  mainLogFile <- file.path(path, "main.log")
  if (!file.exists(mainLogFile)) {
    stop("Gams seems to not have run: 'main.log' does not exist")
  }
  mainLog <- readLines(mainLogFile)
  gdxOutPattern <- "GDX File \\(execute_unload\\)\\s"
  gdxOutLines <- grep(gdxOutPattern, mainLog, value = TRUE)

  gdxPath <- .extractGdxPath(gdxOutLines, gdxOutPattern)

  if (length(gdxPath) == 0) {
    stop("Gams did not write a GDX. This could be due to a compilation or execution error. ",
         "Check the files 'main.log' and 'main.lst' for more details.")
  } else if (length(gdxPath) == 1) {
    if (file.exists(gdxPath)) {
      message("Gams wrote a GDX file to ", gdxPath, ".")
    } else {
      stop("According to 'main.log', Gams wrote a GDX file to ", gdxPath, ", but this file does not exist.")
    }
  } else {
    stop("Gams wrote more than one GDX file: ", gdxPath, ".")
  }

  # Read out run status from gdx
  p_repyFullSysLP <- readSymbol(gdxPath, symbol = "p_repyFullSysLP")
  p_repyFullSysNLP <- readSymbol(gdxPath, symbol = "p_repyFullSysNLP")

  if (isFALSE(silent)) {
    message("Model and solver summary of the linear problem:")
    print(p_repyFullSysLP)

    message("Model and solver summary of the non-linear problem:")
    print(p_repyFullSysNLP)
  }

  if (basename(gdxPath) == "abort.gdx") {
    stop("Gams aborted with \"abort.gdx\" due to errors in at least one variable.")
  }

  # Return success status if NLP finished successfully (or if LP finished successfully in case NLp does not exist)
  successLP <- .checkModelWasSuccessful(p_repyFullSysLP)
  successNLP <- .checkModelWasSuccessful(p_repyFullSysNLP)

  if (!is.null(successNLP)) {
    successNLP
  } else if (!is.null(successLP)) {
    message("Solver and run status for the non-linear problem does not exist. ",
            "Returning the success of the linear problem.")
    successLP
  } else {
    FALSE
  }
}
