#' Create sets
#'
#' Add all sets to gams container based on config
#'
#' @param m gams Container, central object to store all data for input.gdx
#' @param config named list with run configuration
#' @returns gams Container with sets added
#'
#' @author Robin Hasse

createSets <- function(m, config) {

  # Fundamentals ---------------------------------------------------------------

  # sets that are independent of the scenario config

  invisible(m$addSet(
    name = "cost",
    records = c("tangible", "intangible"),
    description = "type of cost"
  ))
  var <- m$addSet(
    name = "var",
    records = c("stock", "construction", "renovation", "demolition"),
    description = "mayor variables of the model"
  )
  invisible(m$addSet(
    name = "qty",
    records = c("area", "num"),
    description = "quantity unit to measure stocks and flows in"
  ))


  # Temporal -------------------------------------------------------------------

  ttotNum <- periodFromConfig(config, "ttot")

  invisible(m$addSet(
    name = "tall",
    records = periodFromConfig(config, "tall"),
    description = "all time steps"
  ))

  ttot <- m$addSet(
    name = "ttot",
    records = ttotNum,
    description = "all modelling time steps"
  )
  invisible(m$addAlias("ttot2", ttot))

  invisible(m$addSet(
    name = "tinit",
    records = periodFromConfig(config, "tinit"),
    description = "initial modelling time step"
  ))
  invisible(m$addSet(
    name = "t",
    records = periodFromConfig(config, "t"),
    description = "modelled time steps"
  ))

  invisible(m$addSet(
    name = "thist",
    records = periodFromConfig(config, "thist"),
    description = "historic time steps"
  ))

  if (grepl("calibration", config[["switches"]][["RUNTYPE"]], fixed = TRUE)) {
    invisible(m$addSet(
      "tcalib",
      records = periodFromConfig(config, "tcalib"),
      description = "time steps considered by the calibration when minimising deviation from target trajectories"
    ))
  }


  # Vintages -------------------------------------------------------------------

  vintages <- getBrickMapping("vintage.csv")

  vin <- m$addSet(
    name = "vin",
    records = unique(getElement(vintages, "vin")),
    description = "construction vintage cohort"
  )

  vinExists <- expandSets(ttot, vin) %>%
    left_join(vintages, by = "vin") %>%
    filter(.data[["ttot"]] > .data[["from"]] - 1) %>%
    select("ttot", "vin")
  vinExists <- m$addSet(
    name = "vinExists",
    domain = c(ttot, vin),
    records = vinExists,
    description = "Can this vintage cohort exist i.e. ttot cannot be before cohort starts"
  )



  # Building state alternatives ------------------------------------------------

  ## building shell ====

  bs <- getBrickMapping("buildingShell.csv") %>%
    getElement("bs") %>%
    unique()
  if (config[["ignoreShell"]]) bs <- head(bs, 1)
  bs <-  m$addSet(
    name = "bs",
    records = bs,
    description = "building shell"
  )

  bsr <- m$addSet(
    name = "bsr",
    records = c(bs$getUELs(), 0),
    description = "renovated building shell"
  )


  ## heating system ====

  hsMap <- getBrickMapping("heatingSystem.csv", "sectoral")

  hs <- hsMap %>%
    getElement("hs") %>%
    unique()
  hs <-  m$addSet(
    name = "hs",
    records = hs,
    description = "heating system"
  )

  hsr <- m$addSet(
    name = "hsr",
    records = c(0, hs$getUELs()),
    description = "renovated heating system"
  )

  carrier <- hsMap %>%
    getElement("carrier") %>%
    unique()
  carrier <- m$addSet(
    name = "carrier",
    records = carrier,
    description = "energy carrier"
  )

  hsCarrier <- unique(hsMap[, c("hs", "carrier")])
  hsCarrier <- m$addSet(
    name = "hsCarrier",
    domain = c(hs, carrier),
    records = hsCarrier,
    description = "mapping between heating system and energy carrier"
  )



  # Independent stock subset ---------------------------------------------------

  region <- m$addSet(
    name = "region",
    records = config[["regions"]],
    description = "region"
  )

  loc <- getBrickMapping("location.csv") %>%
    getElement("loc") %>%
    unique()
  loc <- m$addSet(
    name = "loc",
    records = loc,
    description = "location of building (rural, urban)"
  )

  typMap <- getBrickMapping("buildingType.csv")
  # TODO: remove once dim maps are dynamic
  if (config[["switches"]][["RUNTYPE"]] == "matching") {
    typMap <- typMap %>%
      rbind(data.frame(typ = "Com", subsector = "Com"))
  }

  typ <- typMap %>%
    getElement("typ") %>%
    unique()
  typ <- m$addSet(
    name = "typ",
    records = typ,
    description = "type of residential building (SFH, MFH)"
  )

  inc <- getBrickMapping("incomeQuantile.csv") %>%
    getElement("inc") %>%
    unique()
  inc <- m$addSet(
    name = "inc",
    records = inc,
    description = "income quantile"
  )



  # Other ----------------------------------------------------------------------


  ## Boiler ban ====

  # read ban definition from config
  hsBanConfig <- config[["boilerBan"]] %>%
    listToDf() %>%
    guessColnames(m)

  if (!(is.null(hsBanConfig) || identical(hsBanConfig, "NULL"))) {

    # list banned periods
    hsBanConfig <- hsBanConfig %>%
      group_by(across(everything())) %>%
      mutate(ttot = head(ttotNum, 1)) %>%
      complete(ttot = ttotNum) %>%
      ungroup() %>%
      filter(.data[["ttot"]] > .data[["value"]]) %>%
      select(-"value")
    hsBan <- expandSets(var, region, ttot, hs) %>%
      mutate(across(everything(), as.character)) %>%
      mutate(ttot = as.numeric(.data[["ttot"]]))
    hsBan <- hsBan %>%
      inner_join(hsBanConfig, by = intersect(colnames(hsBan),
                                             colnames(hsBanConfig))) %>%
      select("var", "region", "ttot", "hs")
  } else {
    hsBan <- NULL
  }
  hsBan <- m$addSet(
    name = "hsBan",
    records = hsBan,
    domain = c(var, region, ttot, hs),
    description = "forbidden heating systems in the respective variable in given period"
  )


  ## Allowed renovations ====

  # no decline on energy ladder
  dimMaps <- c(bs = "buildingShell.csv", hs = "heatingSystem.csv")
  renAllowed <- lapply(setNames(nm = names(dimMaps)), function(dim) {
    dimr <- paste0(dim, "r")
    ladder <- getBrickMapping(dimMaps[[dim]]) %>%
      select(all_of(c(dim, "energyLadder")))
    expandSets("bs", "hs", dimr, .m = m) %>%
      left_join(ladder, by = dim) %>%
      left_join(ladder, by = setNames(dim, dimr), suffix = c("Before", "After")) %>%
      mutate(step = replace_na(.data$energyLadderBefore - .data$energyLadderAfter, 0)) %>%
      filter(.data$step >= 0) %>%
      select(all_of(c("bs", "hs", dimr)))
  })


  if (config[["ignoreShell"]]) {
    renAllowed$bs <- renAllowed$bs %>%
      filter(.data$bsr == "0")
  }

  renAllowedBS <- m$addSet(
    name = "renAllowedBS",
    domain = c(bs, hs, bsr),
    records = renAllowed$bs,
    description = "allowed building shell retrofits"
  )

  renAllowedHS <- m$addSet(
    name = "renAllowedHS",
    domain = c(bs, hs, hsr),
    records = renAllowed$hs,
    description = "allowed heating system replacements"
  )


  ## Buildings subsectors ====

  sec <- typMap %>%
    getElement("subsector") %>%
    unique()
  sec <- m$addSet(
    name = "sec",
    records = sec,
    description = "buildings subsector"
  )

  typInSec <- typMap %>%
    select("typ", sec = "subsector") %>%
    unique()
  typInSec <- m$addSet(
    name = "typInSec",
    domain = c(typ, sec),
    records = typInSec,
    description = "mapping between building type and buildings subsector"
  )



  # RETURN ---------------------------------------------------------------------

  return(m)
}
