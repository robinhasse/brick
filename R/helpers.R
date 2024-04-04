#' Make zeros explicit
#'
#' Replace zeros in value column with the gams special value EPS. This way, the
#' value is saved in gdx files by gams.
#'
#' @author Robin Hasse
#'
#' @param x data.frame
#' @param value character, name of value column
#' @returns data.frame with explicit zeros in value column
#'
#' @importFrom gamstransfer SpecialValues

.explicitZero <- function(x, value = "value") {
  x[unlist(lapply(x[[value]] == 0, isTRUE)), value] <- SpecialValues[["EPS"]]
  return(x)
}



#' Filter data frame rows with reference data frames
#'
#' Performs successive semi_joins with passed data frames by all common columns.
#' Useful to filter allowed renovations or vintages that exist.
#'
#' @author Robin Hasse
#'
#' @param x data.frame
#' @param ... data frames that only have columns existing also in \code{x}
#' @returns filtered data frame
#'
#' @importFrom dplyr semi_join

.filter <- function(x, ...) {
  filters <- list(...)
  for (f in filters) {
    if (is.null(f)) next
    missingCols <- setdiff(colnames(f), colnames(x))
    if (length(missingCols) > 0) {
      stop("The following columns of the filter are not present in x: ",
           paste(missingCols, collpase = ", "))
    }
    x <- semi_join(x, f, by = colnames(f))
  }
  x
}



#' Create reference mapping name
#'
#' @author Robin Hasse
#'
#' @param ref character, reference name
#' @param file file extension appended unless set to \code{FALSE}
#' @returns character with reference mapping name

.refMapName <- function(ref, file = FALSE) {
  name <- paste0("refMap_", ref)
  if (!isFALSE(file)) {
    name <- paste(name, file, sep = ".")
  }
  return(name)
}



#' Extract elements that are unique to gams
#'
#' Wrapper around \code{unique} that also drops elements that differ in cases
#' but are identical to gams as it is case-insensitive.
#'
#' @author Robin Hasse
#'
#' @param x a vector or a data frame
#' @returns an object of the same kind but possibly fewer elements. Vectors can
#'   be shorter and data frames can have fewer rows

.unique <- function(x) {
  x <- unique(x)
  x <- if (is.data.frame(x)) {
    x[!duplicated(apply(x, 2, tolower)), ]
  } else {
    x[!duplicated(tolower(x))]
  }
  return(x)
}



#' Insert element between each element of a vector
#'
#' @author Robin Hasse
#'
#' @param x vector
#' @param insert element to insert
#' @returns vector of length \code{length(x) * 2 - 1}

.insertBetween <- function(x, insert) {
  if (!is.vector(x)) {
    stop("'x' has to be a vector.")
  }
  if (length(insert) != 1) {
    stop("'insert' has to be of length 1.")
  }
  result <- c(rbind(x, insert))
  result[-length(result)]
}
