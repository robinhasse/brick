#' Update matching reference selection and weights
#'
#' Read reference configuration from existing run und update the set of selected
#' references and the reference weights in the reference.gdx
#'
#' @param path character, path to run
#' @returns gamstransfer container with updates reference gdx content
#'
#' @author Robin Hasse
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr .data %>% select mutate

reweightMatchingReferences <- function(path) {

  # READ REFERENCES GDX --------------------------------------------------------

  ## find gdx ====

  refGdx <- file.path(path, "references.gdx")

  if (!file.exists(refGdx)) {
    stop("Can't find references.gdx in this path: ", path)
  }

  m <- gamstransfer::Container$new(refGdx)



  ## read gams objects ====


  ref <- m$getSymbols("ref")[[1]]
  reference <- m$getSymbols("reference")[[1]]
  p_refWeight <- m$getSymbols("p_refWeight")[[1]]



  # READ REFERENCE CONFIG ------------------------------------------------------

  refConfigFile <- file.path(path, "config", "references.csv")
  refConfig <- read.csv2(refConfigFile)

  newRef <- refConfig[.isTRUE(refConfig$isUsed), "reference"]
  newRefWeight <- refConfig %>%
    select("reference", value = "weight") %>%
    mutate(value = as.numeric(.data$value))



  # UPDATE GDX -----------------------------------------------------------------

  if (!all(newRef %in% reference$records[[1]])) {
    stop("On or more references used in ", refConfigFile,
         "are not in the gdx ", refGdx)
  }
  ref <- ref$setRecords(reference$records %>%
                          filter(.data$uni %in% newRef))

  p_refWeight$setRecords(newRefWeight)



  # WRITE GDX ------------------------------------------------------------------

  m$write(refGdx)

  return(invisible(m))

}
