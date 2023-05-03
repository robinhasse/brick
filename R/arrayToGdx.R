#' Prepare data frame for GDX
#' 
#' Create an object that can be handled by [gdxrrw::wgdx()] to write the data
#' frame as a gams parameter into a gdx file.
#' 
#' This function takes an array and creates a list ready to be written into a
#' gdx file. It will currently always create an output for a gams parameter.
#' 
#' @author Robin Hasse
#'
#' @param a array with factor columns for gams dimensions and one value
#'   column.
#' @param name character, object name
#' @returns A list with a particular structure required by [gdxrrw::wgdx()].
#' @export
#'
arrayToGdx <- function(a, name) {
  list(name = name,
       val  = a,
       uels = dimnames(a),
       dim  = length(dim(a)),
       form = "full",
       type = "parameter")
}
