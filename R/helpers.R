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
