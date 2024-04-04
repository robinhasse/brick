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
