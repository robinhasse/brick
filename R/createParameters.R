#' Create parameters
#'
#' Add all parameters to gams container based on config
#'
#' @param m gams Container, central object to store all data for input.gdx
#' @param config named list with run configuration
#' @param inputDir directory of input folder
#' @returns gams Container with parameters added
#'
#' @author Robin Hasse

createParameters <- function(m, config, inputDir) {

  # restore set objects
  s <- m$listSets()
  s <- stats::setNames(m$getSymbols(s), s)
  vinExists <- readSymbol(m, "vinExists", stringAsFactor = FALSE)
  stateR <- c("bsr", "hsr")
  state <- c("bs", "hs")
  stateR <- c("bsr", "hsr")
  state <- c("bs", "hs")



  # Periods --------------------------------------------------------------------

  dt <- diff(ttotNum)
  dt <- data.frame(ttot = ttotNum, value = c(dt[1], dt))
  p_dt <- m$addParameter(
    "p_dt",
    "ttot",
    dt,
    description = "length of time step in yr"
  )

  vintages <- getBrickMapping("vintage.csv")
  p_dtVin <- expandSets("ttot", "vin", .m = m) %>%
    left_join(vintages, by = "vin") %>%
    left_join(dt, by = "ttot") %>%
    rename(dt = "value") %>%
    mutate(value = pmax(
      0,
      pmin(.data[["to"]], .data[["ttot"]])
      - pmax(.data[["from"]] - 1, .data[["ttot"]] - .data[["dt"]])
    )) %>%
    select("ttot", "vin", "value")
  p_dtVin <- m$addParameter(
    "p_dtVin",
    c("ttot", "vin"),
    p_dtVin,
    description = "intersection of time step and vintage cohort in yr"
  )

  t0 <- m$addParameter(
    "t0",
    records = as.numeric(s$t$getUELs()[1]),
    description = "reference year for discounting"
  )



  # Specific cost --------------------------------------------------------------


  ## construction ====

  p_specCostConTang <- readInput("f_costConstruction.cs4r",
                                 c("ttot", "reg", "bs", "hs", "typ"),
                                 inputDir) %>%
    toModelResolution(m)
  p_specCostCon <- expandSets("cost", "bs", "hs", "reg", "loc", "typ", "inc", "ttot",
                              .m = m)
  p_specCostConTang <- p_specCostCon %>%
    filter(.data[["cost"]] == "tangible") %>%
    left_join(p_specCostConTang,
              by = c("bs", "hs", "reg", "typ", "ttot"))
  p_specCostConIntang <- p_specCostCon %>%
    filter(.data[["cost"]] == "intangible") %>%
    addAssump(brick.file("assump/costIntangCon.csv"))
  p_specCostCon <- rbind(p_specCostConTang, p_specCostConIntang)
  p_specCostCon <- m$addParameter(
    "p_specCostCon",
    c("cost", state, "reg", "loc", "typ", "inc", "ttot"),
    p_specCostCon,
    description = "floor-space specific construction cost in USD/m2"
  )


  ## renovation ====

  p_specCostRenTang <- readInput("f_costRenovation.cs4r",
                                 c("ttot", "reg", "bs", "hs", "bsr", "hsr",
                                   "typ", "vin"),
                                 inputDir) %>%
    toModelResolution(m)
  p_specCostRen <- expandSets("cost", "bs", "hs", "bsr", "hsr", "vin", "reg",
                              "loc", "typ", "inc", "ttot", .m = m)
  p_specCostRenTang <- p_specCostRen %>%
    filter(.data[["cost"]] == "tangible") %>%
    left_join(p_specCostRenTang,
              by = c("ttot", "reg", "bs", "hs", "bsr", "hsr", "typ", "vin"))
  p_specCostRenIntang <- p_specCostRen %>%
    filter(.data[["cost"]] == "intangible") %>%
    addAssump(brick.file("assump/costIntangRen.csv"))
  p_specCostRen <- rbind(p_specCostRenTang, p_specCostRenIntang)
  p_specCostRen <- m$addParameter(
    "p_specCostRen",
    c("cost", state, stateR, "vin", "reg", "loc", "typ", "inc", "ttot"),
    p_specCostRen,
    description = "floor-space specific renovation cost in USD/m2"
  )


  ## operation ====

  # scenario assumptions
  carbonPrice <- config[["carbonPrice"]]
  carrierPriceLevel <- config[["carrierPrices"]]
  carrierEmiLevel   <- config[["carrierEmi"]]

  # carbon price
  carbonPrice <- if (is.null(carbonPrice)) {
    data.frame(ttot = ttotNum, value = 0)
  } else {
    carbonPrice %>%
      listToDf() %>%
      guessColnames(m) %>%
      toModelResolution(m)
  }
  carbonPrice <- rename(carbonPrice, carbonPrice = "value")

  # energy carrier prices and emission intensities
  hsCarrierMap <- getBrickMapping("heatingSystem.csv") %>%
    select("hs", "carrier")

  carrierData <- readInput("f_carrierPrices.cs4r",
                           c("ttot", "reg", "variable", "unit", "carrier",
                             "level"),
                           inputDir) %>%
    left_join(hsCarrierMap, by = "carrier", relationship = "many-to-many") %>%
    select(-"carrier", -"unit")
  carrierPrices <- carrierData %>%
    filter(.data[["variable"]] == "price") %>%
    pivot_wider(names_from = "variable")
  carrierEmi <- carrierData %>%
    filter(.data[["variable"]] == "emi") %>%
    pivot_wider(names_from = "variable")

  if (!isTRUE(carrierPriceLevel %in% unique(carrierPrices[["level"]]))) {
    stop("The switch 'carrierPrices' has to be exatly one out of [",
         paste(unique(carrierPrices[["level"]]), collapse = ", "), "], not ",
         if (is.null(carrierPriceLevel)) "NULL" else carrierPriceLevel, ".")
  }
  if (!isTRUE(carrierEmiLevel %in% unique(carrierEmi[["level"]]))) {
    stop("The switch 'carrierEmi' has to be exatly one out of [",
         paste(unique(carrierEmi[["level"]]), collapse = ", "), "], not ",
         if (is.null(carrierEmiLevel)) "NULL" else carrierEmiLevel, ".")
  }

  carrierPrices <- carrierPrices %>%
    filter(.data[["level"]] == carrierPriceLevel) %>%
    select(-"level") %>%
    toModelResolution(m, "price")
  carrierEmi <- carrierEmi %>%
    filter(.data[["level"]] == carrierEmiLevel) %>%
    select(-"level") %>%
    toModelResolution(m, "emi")

  # useful energy demand for space heating
  ueDemand <- readInput("f_ueDemand.cs4r",
                        c("reg", "typ", "vin", "bs", "ueDem"),
                        inputDir) %>%
    toModelResolution(m, "ueDem")

  # FE-to-UE-efficiency of heating systems
  eff <- readInput("f_heatingEfficiency.cs4r",
                   c("ttot", "reg", "hs", "typ", "eff"),
                   inputDir) %>%
    toModelResolution(m, "eff")

  # calculate operational cost
  p_specCostOpe <- expandSets("bs", "hs", "vin", "reg", "loc", "typ", ttot = "t",
                              .m = m)
  p_specCostOpe <- p_specCostOpe %>%
    left_join(carrierPrices, by = c("hs", "reg", "ttot")) %>%
    left_join(carrierEmi, by = c("hs", "reg", "ttot")) %>%
    left_join(carbonPrice, by = intersect(colnames(p_specCostOpe),
                                          colnames(carbonPrice))) %>%
    left_join(ueDemand, by = c("bs", "vin", "reg", "typ")) %>%
    left_join(eff, by = c("hs", "reg", "typ", "ttot")) %>%
    mutate(value =
             (.data[["price"]] + .data[["carbonPrice"]] * .data[["emi"]]) *
             .data[["ueDem"]] / .data[["eff"]]) %>%
    select(-"price", -"carbonPrice", -"emi", -"ueDem", -"eff")

  p_specCostOpe <- m$addParameter(
    "p_specCostOpe",
    c(state, "vin", "reg", "loc", "typ", "ttot"),
    p_specCostOpe,
    description = "floor-space specific operation cost in USD/(m2.yr)"
  )


  ## demolition ====

  invisible(m$addParameter(
    "p_specCostDem",
    records = 15,
    description = "floor-space specific demolition cost in USD/m2"
  ))


  # Life time ------------------------------------------------------------------

  # cut off Weibull above this value and assume 1 for technology life time
  cutOffShare <- 0.95

  # read Weibull life time parameters
  lt   <- readInput("f_lifetimeBuilding.cs4r",      c("reg", "typ", "variable"),       inputDir)
  ltBs <- readInput("f_lifetimeBuildingShell.cs4r", c("reg", "variable"),              inputDir)
  ltHs <- readInput("f_lifetimeHeatingSystem.cs4r", c("reg", "typ", "hs", "variable"), inputDir)

  # shift scale parameter of Weibull distribution
  ltHsShift <- config[["ltHsShift"]]
  if (!is.null(ltHsShift)) {
    ltHsShift <- data.frame(hs = names(ltHsShift),
                            variable = "scale",
                            shift = as.numeric(ltHsShift))
    ltHs <- ltHs %>%
      left_join(ltHsShift, by = c("hs", "variable")) %>%
      mutate(value = .data[["value"]] + replace_na(.data[["shift"]], 0)) %>%
      select(-"shift")
  }

  # calculate share of buildings that need to be renovated or demolished between
  # given time steps assuming a Weibull distribution of the technology life time
  shareRen <- function(ttot2, params, standingLifeTime = 0) {

    expandSets(ttot2 = "ttot", "ttot", .m = m) %>%
      left_join(readSymbol(p_dt) %>%
                  rename(dt = "value"),
                by = c(ttot2 = "ttot")) %>%
      cross_join(params) %>%
      pivot_wider(names_from = "variable") %>%
      mutate(lt = .data[["ttot"]] - .data[["ttot2"]]
             + .data[["dt"]] / 2 + standingLifeTime,
             value = pweibull(.data[["lt"]], .data[["shape"]], .data[["scale"]]),
             value = ifelse(.data[["value"]] > cutOffShare,
                            1, .data[["value"]])) %>%
      select(-"shape", -"scale", -"dt", -"lt")
  }

  ## building ====

  # parameter for monitoring purposes
  p_probDem <- expandSets("reg", "typ", ttot2 = "ttot", "ttot", .m = m) %>%
    filter(.data[["ttot2"]] >= .data[["ttot"]]) %>%
    left_join(lt, by = c("reg", "typ"), relationship = "many-to-many") %>%
    pivot_wider(names_from = "variable") %>%
    mutate(value = pweibull(.data[["ttot2"]] - .data[["ttot"]],
                            .data[["shape"]], .data[["scale"]])) %>%
    select(-"shape", -"scale")
  p_probDem <- m$addParameter(
    "p_probDem",
    c("reg", "typ", "ttot2", "ttot"),
    p_probDem,
    description = "probability of a building having reached its end of life"
  )

  # share of stock from previous time step that has to be demolished as it
  # reaches its end of life
  p_shareDem <- expandSets("vin", "reg", "typ", "ttot", .m = m) %>%
    left_join(vintages, by = "vin") %>%
    inner_join(readSymbol(s$vinExists, stringAsFactor = FALSE),
               by = c("vin", "ttot")) %>%
    left_join(lt, by = c("reg", "typ"), relationship = "many-to-many") %>%
    pivot_wider(names_from = "variable") %>%
    mutate(tcon = (.data[["from"]] + pmin(.data[["ttot"]], .data[["to"]])) / 2,
           p = pweibull(.data[["ttot"]] - .data[["tcon"]],
                        .data[["shape"]], .data[["scale"]])) %>%
    left_join(readSymbol(p_dt) %>%
                rename(dt = "value"),
              by = "ttot") %>%
    group_by(across(all_of(c("vin", "reg", "typ")))) %>%
    arrange(.data[["ttot"]]) %>%
    mutate(value = c(0, diff(.data[["p"]])) /
             (1 - lag(.data[["p"]], default = 0)) / .data[["dt"]]) %>%
    select("vin", "reg", "typ", "ttot", "value")
  p_shareDem <- m$addParameter(
    "p_shareDem",
    c("vin", "reg", "typ", "ttot"),
    p_shareDem,
    description = "minimum share of demolition at end of life"
  )

  ## building shell ====

  p_lifeTimeBS <- ltBs %>%
    calc_addVariable(lt = "scale * gamma(1 + 1 / shape)", only.new = TRUE) %>%
    select("reg", "value") %>%
    toModelResolution(m)
  p_lifeTimeBS <- m$addParameter(
    "p_lifeTimeBS",
    "reg",
    p_lifeTimeBS,
    description = "life time of heating system in yr"
  )

  p_shareRenBS <- shareRen(s$ttot, ltBs) %>%
    select("reg", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenBS <- m$addParameter(
    "p_shareRenBS",
    c("reg", "ttot2", "ttot"),
    p_shareRenBS,
    description = "minimum share of renovation from the building shell reaching end of life"
  )

  p_shareRenBSinit <- shareRen(s$ttot, ltBs, 12) %>%
    select("reg", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenBSinit <- m$addParameter(
    "p_shareRenBSinit",
    c("reg", "ttot2", "ttot"),
    p_shareRenBSinit,
    description = "minimum share of renovation from the building shell of initial stock reaching end of life"
  )

  ## heating system ====

  p_lifeTimeHS <- ltHs %>%
    calc_addVariable(lt = "scale * gamma(1 + 1 / shape)", only.new = TRUE) %>%
    select("hs", "reg", "typ", "value") %>%
    toModelResolution(m)
  p_lifeTimeHS <- m$addParameter(
    "p_lifeTimeHS",
    c("hs", "reg", "typ"),
    p_lifeTimeHS,
    description = "life time of heating system in yr"
  )

  p_shareRenHS <- shareRen(s$ttot, ltHs) %>%
    select("hs", "reg", "typ", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenHS <- m$addParameter(
    "p_shareRenHS",
    c("hs", "reg", "typ", "ttot2", "ttot"),
    p_shareRenHS,
    description = "minimum share of renovation from the heating system reaching end of life"
  )

  p_shareRenHSinit <- shareRen(s$ttot, ltHs, 6) %>%
    select("hs", "reg", "typ", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenHSinit <- m$addParameter(
    "p_shareRenHSinit",
    c("hs", "reg", "typ", "ttot2", "ttot"),
    p_shareRenHSinit,
    description = "minimum share of renovation from the heating system of initial stock reaching end of life"
  )


  # Other ----------------------------------------------------------------------


  ## discount factor ====

  p_discountFac <- expandSets("typ", "ttot", .m = m) %>%
    left_join(readSymbol(p_dt) %>%
                rename(dt = "value"),
              by = "ttot") %>%
    mutate(r = c(SFH = 0.21, MFH = 0.25)[.data[["typ"]]], # Giraudet et al. 2012
           value = 1 / (1 + .data[["r"]])^(.data[["ttot"]] - .data[["dt"]] / 2
                                           - unlist(readSymbol(t0)))) %>%
    select("typ", "ttot", "value")
  p_discountFac <- m$addParameter(
    "p_discountFac",
    c("typ", "ttot"),
    p_discountFac,
    description = "discount factor w.r.t. t0"
  )


  ## population ====

  # SSP scenario
  popScenario <- config[["popScenario"]]

  # read pop data
  pop <- readInput("f_population.cs4r",
                   c("ttot", "reg", "scenario", "loc", "typ"),
                   inputDir)

  if (!isTRUE(popScenario %in% unique(pop[["scenario"]]))) {
    stop("The switch 'popScenario' has to be exatly one out of [",
         paste(unique(pop[["scenario"]]), collapse = ", "), "], not ",
         if (is.null(popScenario)) "NULL" else popScenario, ".")
  }

  pop <- pop %>%
    filter(.data[["scenario"]] == popScenario) %>%
    select(-"scenario") %>%
    toModelResolution(m)

  p_population <- expandSets("reg", "loc", "typ", "inc", "ttot", .m = m) %>%
    left_join(pop, by = c("reg", "typ", "loc", "ttot"),
              relationship = "many-to-many") %>%
    select("reg", "loc", "typ", "inc", "ttot", "value")

  p_population <- m$addParameter(
    "p_population",
    c("reg", "loc", "typ", "inc", "ttot"),
    p_population,
    description = "number of people in million"
  )


  ## floor space per capita ====

  # SSP scenario
  fsScenario <- config[["fsScenario"]]

  # read floor space data
  p_floorPerCap <- readInput("f_floorspacePerCap.cs4r",
                             c("ttot", "reg", "scenario", "typ", "loc"),
                             inputDir)

  # pick scenario
  if (!isTRUE(fsScenario %in% unique(p_floorPerCap[["scenario"]]))) {
    stop("The switch 'fsScenario' has to be exatly one out of [",
         paste(unique(p_floorPerCap[["scenario"]]), collapse = ", "), "], not ",
         if (is.null(fsScenario)) "NULL" else fsScenario, ".")
  }

  p_floorPerCap <- p_floorPerCap %>%
    filter(.data[["scenario"]] == fsScenario) %>%
    select(-"scenario") %>%
    toModelResolution(m)

  p_floorPerCap <- expandSets("reg", "loc", "typ", "inc", "ttot", .m = m) %>%
    left_join(p_floorPerCap, by = c("reg", "loc", "typ", "ttot"))

  p_floorPerCap <- m$addParameter(
    "p_floorPerCap",
    c("reg", "loc", "typ", "inc", "ttot"),
    p_floorPerCap,
    description = "floor space per capita in m2"
  )


  # Stock ----------------------------------------------------------------------

  # stock of residential floor space
  p_stockHist <- readInput("f_buildingStock.cs4r",
                           c("ttot", "reg", "variable", "typ", "loc", "vin", "hs"),
                           inputDir) %>%
    filter(.data[["variable"]] == "floor") %>%
    select(-"variable") %>%
    mutate(bs  = "low",
           qty = "area")
  p_stockHist <- expandSets("qty", "bs", "hs", "vin", "reg", "loc", "typ",
                            "inc", "ttot", .m = m) %>%
    inner_join(readSymbol(s$vinExists, stringAsFactor = FALSE),
               by = c("vin", "ttot")) %>%
    left_join(p_stockHist,
              by = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "ttot")) %>%
    mutate(value = replace_na(.data[["value"]], 0))

  p_stockHist <- m$addParameter(
    "p_stockHist",
    c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", "ttot"),
    p_stockHist,
    description = "historic stock of buildings in million m2"
  )


  return(m)
}
