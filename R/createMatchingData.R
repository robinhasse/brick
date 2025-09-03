#' Create data for reference matching
#'
#' This function also performs auto gams code generation to load the reference
#' sets and parameters
#'
#' @author Robin Hasse
#'
#' @param path character vector with folders to run the model in
#' @param config run configurations
#' @param overwrite logical, should existing data be overwritten?
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom quitte as.quitte
#' @importFrom tidyr complete
#' @importFrom dplyr select rename %>% .data mutate group_by ungroup summarise
#'   filter matches any_of
#' @importFrom utils read.csv read.csv2
#' @importFrom stats median
#' @importFrom gamstransfer Container
#' @export
#'
createMatchingData <- function(path, config, overwrite = FALSE) {

  # FUNCTIONS ------------------------------------------------------------------

  getRefs <- function() {
    refListName <- "references.csv"
    refListPath <- file.path(path, "config", refListName)
    if (file.exists(refListPath)) {
      refList <- toolGetMapping(refListPath, type = NULL, where = "local",
                                returnPathOnly = TRUE) %>%
        read.csv2(comment.char = "#")
    } else {
      refList <- toolGetMapping(refListName,
                                type = "sectoral", where = "mredgebuildings",
                                returnPathOnly = TRUE) %>%
        read.csv2(comment.char = "#")
      write.csv2(refList, refListPath, row.names = FALSE)
    }
    return(refList)
  }



  getRefData <- function(ref, data, regions, periods) {
    data %>%
      as.quitte(na.rm = TRUE) %>%
      filter(.data[["region"]] %in% regions,
             .data[["period"]] %in% periods) %>%
      group_by(across(-all_of(c("region", "period", "value")))) %>%
      complete(region = regions, period = periods) %>%
      ungroup() %>%
      mutate(reference = ref) %>%
      select("reference",
             refVar = "variable",
             region = "region",
             ttot = "period",
             "value") %>%
      .explicitZero()
  }





  getRefDescription <- function(data) {

    getField <- function(x, key) {
      keyPattern <- paste0("^\\s*", key, ":\\s*")
      line <- grep(keyPattern, x)
      sub(keyPattern, "", x[line])
    }

    comment <- attr(data, "comment")
    description <- getField(comment, "description")
    unit <- getField(comment, "unit")

    paste0(description, " [", unit, "]")
  }



  getRefMap <- function(ref) {
    toolGetMapping(name = .refMapName(ref, "csv"),
                   type = "sectoral",
                   where = "mredgebuildings",
                   returnPathOnly = TRUE) %>%
      read.csv(comment.char = "#", check.names = FALSE) %>%
      select(-matches("^\\.")) %>%
      rename(refVar = "variable") %>%
      unique()
  }


  getBasicMapping <- function(refMaps) {
    do.call(rbind, lapply(names(refMaps), function(ref) {
      refMaps[[ref]] %>%
        select("refVar", "refVarGroup") %>%
        .unique() %>%
        mutate(reference = ref, .before = "refVar")
    }))
  }


  getRefVars <- function(refMaps) {
    lapply(refMaps, getElement, "refVar") %>%
      unlist() %>%
      .unique()
  }


  tidyRefMap <- function(refMap) {
    refMap %>%
      select(-any_of("refVarGroup"))
  }



  # PREPARE --------------------------------------------------------------------

  # check file path
  refFilePath <- file.path(path, "references.gdx")
  if (file.exists(refFilePath)) {
    if (overwrite) {
      warning("References file '", refFilePath, "' overwritten.")
    } else {
      stop("References file '", refFilePath,
           "' cannot be created as it already exists.")
    }
  }

  regions <- config[["regions"]]
  periods <- config[["periods"]]

  m <- Container$new()



  # READ DATA ------------------------------------------------------------------

  ## references ====

  refConfig <- getRefs()
  references <- refConfig[["reference"]]
  referencesRel <- refConfig[.isTRUE(refConfig[["isRelative"]]), "reference"]
  referencesAbs <- setdiff(references, referencesRel)

  referencesUsed <- refConfig %>%
    filter(.data[["isUsed"]]) %>%
    getElement("reference")


  ## reference mappings ====

  refMapsFull <- lapply(setNames(nm = references), getRefMap)
  refMaps <- lapply(refMapsFull, tidyRefMap)
  refVarBasic <- getBasicMapping(refMapsFull[referencesRel])
  refVarGroup <- .unique(refVarBasic[["refVarGroup"]])


  ## reference weights ====

  refWeight <- refConfig %>%
    select("reference", value = "weight") %>%
    mutate(value = as.numeric(.data$value))


  ## reference values ====

  # TODO: this should come from refs
  refVals <- data.frame()
  refRecords <- data.frame()
  for (ref in references) {
    refData <- calcOutput("MatchingReference", subtype = ref, aggregate = FALSE)
    refRecords <- rbind(refRecords,
                        data.frame(reference = ref,
                                   description = getRefDescription(refData)))
    refVals <- rbind(refVals,
                     getRefData(ref, refData, regions, periods))
  }

  refValsMed <- refVals %>%
    group_by(across(all_of(c("reference", "region")))) %>%
    summarise(value = median(abs(.data[["value"]][.data[["value"]] != 0]),
                             na.rm = TRUE),
              .groups = "drop")

  # data-dependent sets
  refVarExists <- refVals[!is.na(refVals$value),
                          c("reference", "refVar", "region", "ttot")] %>%
    .unique()
  refVarGroupExists <- refVarExists %>%
    inner_join(refVarBasic, by = c("reference", "refVar")) %>%
    select("reference", "refVarGroup", "region", "ttot") %>%
    .unique()

  # mapping references to refVals
  refVarRef <- refVals %>%
    select("reference", "refVar") %>%
    .unique()

  # ignore refVars starting with _
  refVarConsidered <- refVarRef %>%
    filter(!startsWith(as.character(.data$refVar), "_"))



  # CREATE GAMS OBJECTS --------------------------------------------------------

  for (ref in references) {
    invisible(m$addSet(
      name = .refMapName(ref),
      records = refMaps[[ref]],
      domain = colnames(refMaps[[ref]]),
      domainForwarding = TRUE,
      description = paste("Mapping of brick variables to reference variables:",
                          ref)
    ))
  }

  invisible(m$addParameter(
    name = "p_refVals",
    domain = c("reference", "refVar", "region", "ttot"),
    records = refVals,
    domainForwarding = TRUE,
    description = "Reference values to match"
  ))

  invisible(m$addParameter(
    name = "p_refValsMed",
    domain = c("reference", "region"),
    records = refValsMed,
    domainForwarding = TRUE,
    description = "Median of all reference values for one reference in one region"
  ))

  invisible(m$addParameter(
    name = "p_refWeight",
    domain = "reference",
    records = refWeight,
    domainForwarding = TRUE,
    description = "Weight of reference"
  ))

  refVar <- m$addSet(
    name = "refVar",
    records = getRefVars(refMaps),
    description = "variables of reference sources"
  )

  reference <- m$addSet(
    name = "reference",
    records = refRecords,
    description = "reference sources that historic quantities can be calibrated to"
  )
  invisible(m$addSet(
    name = "refAbs",
    records = refRecords[refRecords$reference %in% referencesAbs, ],
    description = "absolute reference sources that historic quantities can be calibrated to"
  ))
  invisible(m$addSet(
    name = "refRel",
    records = refRecords[refRecords$reference %in% referencesRel, ],
    description = "relative reference sources that historic quantities can be calibrated to"
  ))
  invisible(m$addSet(
    name = "refVarGroup",
    records = refVarGroup,
    description = "group of reference variables"
  ))

  invisible(m$addSet(
    name = "ref",
    records = refRecords[refRecords$reference %in% referencesUsed, ],
    description = "reference sources that historic quantities are calibrated to"
  ))
  refVarRef <- m$addSet(
    name = "refVarRef",
    domain = c(reference, refVar),
    records = refVarRef,
    description = "mapping references to reference variables"
  )
  refVarConsidered <- m$addSet(
    name = "refVarConsidered",
    domain = c(reference, refVar),
    records = refVarConsidered,
    description = "mapping references to all reference variables that are considered in the deviation"
  )

  refVarExists <- m$addSet(
    name = "refVarExists",
    domain = c("reference", "refVar", "region", "ttot"),
    records = refVarExists,
    description = "There is a value for this combination of reference, variable, region and period"
  )

  refVarGroupExists <- m$addSet(
    name = "refVarGroupExists",
    domain = c("reference", "refVarGroup", "region", "ttot"),
    records = refVarGroupExists,
    description = "There is a value for this combination of reference, variable group, region and period"
  )

  invisible(m$addSet(
    name = "refVarBasic",
    domain = c("reference", "refVar", "refVarGroup"),
    records = refVarBasic,
    description = paste("mapping each reference variables to all reference",
                        "variables summed to basic value of a share reference")
  ))

  m$write(refFilePath, compress = TRUE)


}
