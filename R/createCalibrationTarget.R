#' Create calibration targets
#'
#' read results from matching run and aggregate them spatially and temporally
#' to the resolution of calibration runs
#'
#' This is a first implementation with many known issues/limitations:
#' \itemize{
#'  \item The temporal resolution is hard-coded.
#'  \item It is assumed that the matching ran on European countries and is to be
#'  aggregated to one EU27 region
#'  \item The temporal aggregation of renovation flows is non-trivial: It is
#'  generally possible that buildings can change states multiple time within one
#'  aggregated time step but they can't be tracked through renovations
#'  individually. Currently, we just assume the mean of renovations across all
#'  years of an aggregated time step and adjust the zero flows to match the
#'  stock. This procedure can lead to inconsistent and even negative flows. In
#'  fact, aggregated stock and flows are inconsistent in the current version.
#' }
#'
#' @author Robin Hasse
#'
#' @param path character, directory of matching folder
#' @param outDir character, directory to save files in
#'
#' @importFrom gamstransfer Container
#' @importFrom utils write.table
#' @export

createCalibrationTarget <- function(path, outDir) {

  # CONFIG ---------------------------------------------------------------------

  periods <- c(2000, 2005, 2010, 2015, 2020)

  dt <- data.frame(ttotAgg = periods, dt = c(NA, diff(periods)))

  periodMap <- dt %>%
    filter(!is.na(.data$dt)) %>%
    group_by(.data$ttotAgg) %>%
    reframe(ttot = seq(to = .data$ttotAgg, length.out = .data$dt))



  # READ DATA ------------------------------------------------------------------

  m <- Container$new(file.path(path, "output.gdx"))

  dtVin <- readSymbol(m, "p_dtVin") %>%
    rename(ttotAgg = "ttot") %>%
    right_join(periodMap, by = "ttotAgg") %>%
    group_by(across(all_of(c("ttotAgg", "vin")))) %>%
    summarise(dtVin = sum(.data$value), .groups = "drop")

  vars <- c(
    stock = "v_stock",
    construction = "v_construction",
    renovation = "v_renovation",
    demolition = "v_demolition"
  )
  flows <- c("construction", "renovation", "demolition")

  v <- lapply(vars, readSymbol, x = m, selectArea = FALSE)



  # AGGREGATION ----------------------------------------------------------------

  ## temporal ====

  v$stock <- v$stock %>%
    filter(.data$ttot %in% periods)

  v[flows] <- lapply(v[flows], function(flow) {
    flow %>%
      filter(.data$qty == "area") %>%
      right_join(periodMap, by = "ttot") %>%
      group_by(across(-all_of(c("ttot", "value")))) %>%
      summarise(value = mean(.data$value), .groups = "drop") %>%
      rename(ttot = "ttotAgg")
  })

  # recalculate zero renovation flows that fulfil the stock balance
  v$renovation <- v$renovation %>%
    left_join(dt, by = c(ttot = "ttotAgg")) %>%
    left_join(dtVin, by = c(ttot = "ttotAgg", "vin")) %>%
    mutate(dtVin = replace_na(.data$dtVin, 0),
           ttotPrev = .data$ttot - .data$dt,
           untouched = .data$bsr == "0" & .data$hsr == "0") %>%
    left_join(v$stock,
              by = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", ttotPrev = "ttot"),
              suffix = c("", "StockPrev")) %>%
    left_join(v$construction,
              by = c("qty", "bs", "hs", "reg", "loc", "typ", "inc", "ttot"),
              suffix = c("", "Construction"))  %>%
    group_by(across(all_of(c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", "ttot")))) %>%
    mutate(value = ifelse(.data$untouched,
                          .data$valueStockPrev / .data$dt +
                            .data$valueConstruction * .data$dtVin / .data$dt -
                            sum(.data$value[!.data$untouched]),
                          .data$value)) %>%
    ungroup() %>%
    select(all_of(names(v$renovation)))


  ## spatial ====

  v <- lapply(v, function(x) {
    x %>%
      group_by(across(-all_of(c("reg", "value")))) %>%
      summarise(value = signif(sum(.data$value), 4)) %>%
      mutate(reg = "EUR", .before = "loc")
  })



  # OUTPUT ---------------------------------------------------------------------

  if (!dir.exists(outDir)) {
    dir.create(outDir)
  }

  header <- paste("* matching run:", path)
  lapply(names(v), function(var) {
    outFile <- file.path(outDir, paste0("f_", var, "CalibTarget.cs4r"))
    writeLines(header, outFile)
    write.table(v[[var]], outFile, append = TRUE, quote = FALSE, sep = ",",
                row.names = FALSE, col.names = FALSE)
  })

  return(invisible(v))
}
