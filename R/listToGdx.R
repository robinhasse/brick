#' Prepare data frame for GDX
#' 
#' Create an object that can be handled by [gdxrrw::wgdx()] to write the data
#' frame as a gams set into a gdx file.
#' 
#' This function takes an array and creates a list ready to be written into a
#' gdx file. It will currently always create an output for a gams parameter.
#' 
#' @author Robin Hasse
#'
#' @param setList List of character vectors set names as names and their
#'   respective elements as character vector
#' @returns A list with a particular structure required by [gdxrrw::wgdx()].
#' @export
#'
listToGdx <- function(setList) {
  lapply(names(setList), function(s) {
    list(name = s,
         uels = setList[s])
  })
}
