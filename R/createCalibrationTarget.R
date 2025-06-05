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
#' @param digits integer indicating the number of decimal places
#'
#' @importFrom gamstransfer Container
#' @importFrom utils write.table
#' @importFrom dplyr .data %>% mutate filter select group_by across all_of
#'   ungroup reframe group_modify
#' @importFrom tidyr replace_na
#' @export

createCalibrationTarget <- function(path, outDir, digits = 4) {

  # CONFIG ---------------------------------------------------------------------

  periods <- c(2000, 2005, 2010, 2015, 2020)

  dt <- data.frame(ttotAgg = periods, dt = c(NA, diff(periods)))

  periodMap <- dt %>%
    filter(!is.na(.data$dt)) %>%
    group_by(.data$ttotAgg) %>%
    reframe(ttot = seq(to = .data$ttotAgg, length.out = .data$dt))



  # FUNCTIONS ------------------------------------------------------------------

  relSSE <- function(x, y) {
    if (all(x == y)) {
      return(0)
    }
    sum((x - y)^2) / sum(x^2)
  }

  joinVar <- function(x, v, var, by = NULL, valueSuffix = NULL) {
    dataVar <- v[[var]]
    names(dataVar)[names(dataVar) == "value"] <- paste0(var, valueSuffix)
    cols <- intersect(names(x), names(dataVar))
    cols <- c(cols[!cols %in% by], by)
    dplyr::left_join(x, dataVar, by = cols)
  }


  # find renovation transition closest to average renovation while satisfying
  # stock balances
  correctRenovation <- function(x, key, maxAttempts = 5, maxDeviation = 1E-4) {

    keyMsg <- paste(names(key), lapply(key[1, ], as.character),
                    sep = " = ", collapse = ", ")

    identityMatrix <- diag(nrow(x))

    stockBal <- c("Prev", "Next")

    constraintMatrix <- do.call(cbind, lapply(stockBal, function(constraint) {
      stats::model.matrix(
        stats::reformulate(paste0("state", constraint), intercept = FALSE),
        data = x
      )
    }))

    constraintRhs <- do.call(c, lapply(stockBal, function(constraint) {
      rhs <- unique(x[paste0(c("state", "rhs"), constraint)])
      setNames(rhs[[2]], paste0("state", constraint, rhs[[1]]))
    }))

    attempt <- 0
    errors <- c()

    for (i in order(constraintRhs, decreasing = TRUE)) {

      attempt <- attempt + 1

      # remove one constraint such that remaining constraints are independent
      constraintsIndep <- names(constraintRhs)[-i]
      constraintRhsIndep <- constraintRhs[constraintsIndep]
      constraintMatrixIndep <- constraintMatrix[, constraintsIndep]

      # solve
      r <- tryCatch(
        quadprog::solve.QP(Dmat = identityMatrix,
                           dvec = x$estimate,
                           Amat = cbind(constraintMatrixIndep, identityMatrix),
                           bvec = c(constraintRhsIndep, rep(0, nrow(x))),
                           meq = length(constraintRhsIndep)),
        error = conditionMessage
      )

      if (length(r) == 1) {
        errors <- c(errors, r)
        if (attempt >= maxAttempts) {
          stop("Unable to solve optimisation after ", attempt,
               " attempts for this stock subset:\n  ", keyMsg, "\n",
               "Solving errors:\n  ", paste(errors, collapse = "\n  "),
               call. = FALSE)

        }
      } else {
        break
      }
    }

    # check how far the optimisation result is from the estimate
    deviation <- relSSE(x$estimate, r$solution)
    if (deviation > maxDeviation) {
      warning("Optimisation result is far away from estimate ",
              "for this stock subset:\n  ", keyMsg, "\n",
              "Deviation: ", signif(deviation, 2), "\n")
    }

    x$value <- pmax(0, r$solution)
    x
  }



  # READ DATA ------------------------------------------------------------------

  m <- Container$new(file.path(path, "output.gdx"))

  dtVin <- readSymbol(m, "p_dtVin") %>%
    rename(ttotAgg = "ttot") %>%
    right_join(periodMap, by = "ttotAgg") %>%
    group_by(across(all_of(c("ttotAgg", "vin")))) %>%
    summarise(dtVin = sum(.data$value), .groups = "drop")

  renAllowed <- readSymbol(m, "renAllowed")

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
    rename(renovation = "value") %>%
    semi_join(renAllowed, by = c("bs", "hs", "bsr", "hsr")) %>%
    left_join(dt, by = c(ttot = "ttotAgg")) %>%
    left_join(dtVin, by = c(ttot = "ttotAgg", "vin")) %>%
    # ttot are the aggregated periods from here on
    mutate(across(all_of(c("bs", "hs", "bsr", "hsr")), as.character),
           .bsr = ifelse(.data$bsr == "0", .data$bs, .data$bsr),
           .hsr = ifelse(.data$hsr == "0", .data$hs, .data$hsr),
           statePrev = paste(.data$bs, .data$hs),
           stateNext = paste(.data$.bsr, .data$.hsr),
           dtVin = replace_na(.data$dtVin, 0),
           ttotPrev = .data$ttot - .data$dt,
           untouched = .data$bsr == "0" & .data$hsr == "0") %>%
    joinVar(v, "stock", by = c(.bsr = "bs", .hsr = "hs"), valueSuffix = "Next") %>%
    joinVar(v, "stock", by = c(ttotPrev = "ttot"), valueSuffix = "Prev") %>%
    joinVar(v, "construction") %>%
    joinVar(v, "demolition", by = c(.bsr = "bs", .hsr = "hs")) %>%
    replace_na(list(stockPrev = 0)) %>%
    mutate(rhsPrev = .data$stockPrev / .data$dt + .data$construction * .data$dtVin / .data$dt,
           rhsNext = .data$stockNext / .data$dt + .data$demolition) %>%
    group_by(across(all_of(setdiff(names(v$renovation), c("hsr", "value"))))) %>%
    mutate(estimate = ifelse(.data$untouched,
                             .data$rhsPrev - sum(.data$renovation[!.data$untouched]),
                             .data$renovation)) %>%
    group_by(across(all_of(c("qty", "vin", "region", "loc", "typ", "inc", "ttot")))) %>%
    group_modify(correctRenovation) %>%
    ungroup() %>%
    select(all_of(names(v$renovation)))

  ## spatial ====

  v <- lapply(v, function(x) {
    x %>%
      group_by(across(-all_of(c("region", "value")))) %>%
      summarise(value = round(sum(.data$value), digits)) %>%
      mutate(region = "EUR", .before = "loc")
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

  message("Calibration targets written to ", outDir, ".\n")

  return(invisible(v))
}
