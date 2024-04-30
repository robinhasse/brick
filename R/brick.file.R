#' Find the full file names of files in BRICK
#'
#' This is meant to work with the installed package BRICK but also when loading
#' the package via \code{devtools::load_all("path/to/brick")}
#'
#' @param ... character vectors, specifying subdirectory and files within brick
#' @returns A character vector of positive length, containing the file paths
#'   that matched \code{...}, or the empty string, \code{""}, if none matched
#'   (unless \code{mustWork = TRUE}).
#'   files.
#'
#' @author Robin Hasse
#'
#' @importFrom piamutils getSystemFile
#' @export

brick.file <- function(...) {
  path <- getSystemFile(file.path(...), package = "brick")
  if (!file.exists(path)) {
    stop("Cannot find this BRICK file: ", path)
  }
  return(path)
}
