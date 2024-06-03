#' Copy history gdx to output folder
#'
#' @param path character vector with folders to write input data into
#' @param config named list with run configuration
#' @param overwrite logical, should existing input.gdx be overwritten?
#' @param thistOnly logical, crop temporal parameters to historic periods
#'
#' @author Robin Hasse
#'
#' @importFrom gamstransfer Container SpecialValues

copyHistoryGdx <- function(path, config, overwrite = FALSE, thistOnly = TRUE) {

  # find file
  originGdx <- config[["historic"]]
  if (length(originGdx) > 1) {
    stop("Don't give more than one historical. You gave:\n",
         paste(originGdx, collapse = "\n"))
  }
  originGdxFile <- .findOriginGdxFile(originGdx)

  # copy file
  targetGdxFile <- file.path(path, "history.gdx")
  if (!is.null(originGdxFile)) {
    file.copy(originGdxFile, targetGdxFile,
              overwrite = overwrite)
    message("Using ", originGdxFile, " as history.gdx")
  }

  # crop all temporal parameters to thist
  if (isTRUE(thistOnly) && !is.null(targetGdxFile)) {
    thist <- periodFromConfig(config, "historic")
    .cropParamsToThist(targetGdxFile, thist)
    message("Cropped all temporal parameters in ", targetGdxFile,
            " to historic time steps: ", paste(thist, collapse = ", "))
  }

  return(invisible(targetGdxFile))

}





#' find origin gdx file
#'
#' @param originGdx character, file path to run or gdx file used as historical
#' @returns file path to origin gdx file

.findOriginGdxFile <- function(originGdx) {
  # recognised file names for inital gdx (in order of priority)
  gdxNames <- c("output.gdx")

  originGdxFiles <- file.path(originGdx, gdxNames)

  if (file.exists(originGdx) && !dir.exists(originGdx)) {
    return(originGdx)
  }
  if (dir.exists(originGdx)) {
    if (any(file.exists(originGdxFiles))) {
      return(head(originGdxFiles[which(file.exists(originGdxFiles))], 1))
    } else {
      stop("There is no file for the historical in the given directory: ",
           originGdx)
    }
  }
  stop("Unable to find a file for the historical with given config ",
       "parameter: ", originGdx)
}





#' crop gdx parameters to historic periods
#'
#' remove all records of temporal parameters that are outside of \code{thist}
#' and make zero values explicit filling with EPS.
#'
#' @param gdx character, file path to gdx
#' @param thist numeric vector of historic periods
#'
.cropParamsToThist <- function(gdx, thist) {
  m <- Container$new(gdx)

  missingPeriods <- setdiff(thist, readSymbol(m, "ttot")[[1]])
  if (length(missingPeriods) > 0) {
    stop(gdx, " is missing historic periods: ",
         paste(missingPeriods, collapse = ", "))
  }

  for (paramName in m$listParameters()) {
    param <- m$getSymbols(paramName)[[1]]
    if (!"ttot" %in% param$domainLabels) {
      next
    }
    param$records <- param$records %>%
      filter(.data[["ttot"]] %in% thist) %>%
      mutate(value = ifelse(.data[["value"]] == 0,
                            SpecialValues[["EPS"]],
                            .data[["value"]]))
  }
  m$write(gdx, compress = TRUE)

  return(invisible(gdx))
}
