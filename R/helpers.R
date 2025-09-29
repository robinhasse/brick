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
  x[.isTRUE(x[[value]] == 0), value] <- SpecialValues[["EPS"]]
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



#' Test if vector elements are TRUE
#'
#' Perform \code{isTRUE} on each vecotr element
#'
#' @author Robin Hasse
#'
#' @param x vector
#' @returns logical vector

.isTRUE <- function(x) {
  unlist(lapply(x, isTRUE))
}



#' Pick lines from data.frame by identifiers
#'
#' @param x data.frame
#' @param ... <column> = <selectionElement>
#' @param .colsMustExist logical, if \code{TRUE}, an error is thrown for missing
#'   columns
#' @returns data.frame with filtered rows without identifier columns

.pick <- function(x, ..., .colsMustExist = TRUE) {
  lst <- list(...)
  for (col in names(lst)) {
    if (.colsMustExist && !col %in% names(x)) {
      stop("There is no column '", col, "'.")
    }
    x <- x[x[[col]] %in% lst[[col]], setdiff(names(x), col), drop = FALSE]
  }
  return(x)
}



#' Lapply with output named by given list
#'
#' @param nm list of characters, lapply input list and name of output
#' @param ... further inputs to lapply
#' @returns named list
#'
.namedLapply <- function(nm, ...) {
  stats::setNames(lapply(X = nm, ...), nm)
}


#' Add time stamp
#'
#' Append current date and time
#'
#' @param x character
#' @returns character with time stamp suffix

.addTimeStamp <- function(x) {
  stamp <- format(Sys.time(), "_%Y-%m-%d_%H.%M.%S")
  paste0(x, stamp)
}



#' Find region mapping
#'
#' @param regionmapping character. If \code{NULL}, country regions are used. It
#'   can otherwise be a path to a mapping file (length one) or a vector of
#'   length two with the file name and the location (\code{where}) of a madrat
#'   regional mapping.
#' @returns data frame with region mapping
#'
#' @importFrom madrat toolGetMapping

.findRegionMapping <- function(regionmapping) {

  if (is.null(regionmapping)) {
    # missing region mapping -> national resolution
    name <- "regionmappingNational.csv"
    where <- "mredgebuildings"
  } else if (is.character(regionmapping)) {
    name <- regionmapping[1]
    if (length(regionmapping) == 1) {
      if (!file.exists(name)) {
        stop("Can't find regionmapping. This file doesn't exist: ", name)
      }
      where <- "local"
    } else if (length(regionmapping) == 2) {
      where <- regionmapping[2]
    } else {
      stop("'regionmapping' in your config cannot have more than two elements.",
           " Either give the directory to a local mapping file or a vector of ",
           "the file name and the 'where' argument of toolGetMapping.")
    }
  } else {
    stop("'regionmapping' in your config can either be NULL, a filepath or a ",
         "character vector of length 2, not ", class(regionmapping), ".")
  }

  toolGetMapping(name, "regional", where)
}
