#' Compute the share of heating systems to be renovated
#'
#' Calculate share of buildings that need to be renovated or demolished between
#' given time steps assuming a Weibull distribution of the technology life time.
#'
#' If \code{standingLifeTime = TRUE}, a standing stock is considered.
#' We differentiate 3 levels assuming either increasing, constant or decreasing
#' temporal profile of the installations that built to the standing stock. We
#' assume installations changed quadratically with the age of the system going
#' back from the initial time step to the start of the vintage cohort, but at
#' most 25 years.
#'
#' @param params data frame with parameters of the Weibull distribution
#' @param m Gams container with at least \code{ttotIn}, \code{ttotOut},
#'   \code{tinit} and \code{p_dt}
#' @param cutOffShare numeric, renovation share at which we cut off the
#'   distribution function by jumping to 1
#' @param standingLifeTime logical, whether standing lifetime needs to be
#'   considered
#' @param timePeriods data frame with time periods for which the calculations
#'   should be done
#' @param vintages data frame with vintages and their starting and end year
#'
#' @returns Data frame of shares to be renovated in each time period
#'
#' @importFrom dplyr %>% .data across any_of cross_join everything filter
#'   group_by left_join mutate rename right_join select ungroup
#' @importFrom tidyr pivot_wider
#'
computeShareRen <- function(params,
                            m,
                            cutOffShare = 1,
                            standingLifeTime = FALSE,
                            timePeriods = NULL,
                            vintages = NULL,
                            levels = "central") {



  # Functions ------------------------------------------------------------------

  # Weibull probability distribution function with shifted argument
  .shiftedPweibull <- function(x, shift, shape, scale, support = NULL) {
    stats::pweibull(x + shift, shape, scale)
  }

  # Density of rescaled Beta(3,1) distribution
  # multiplied with shifted Weibull distribution function
  .increasingBeta <- function(x, shift, shape, scale, support) {
    (x / support)^2 / beta(3, 1) * .shiftedPweibull(x, shift, shape, scale)
  }

  # Density of rescaled Beta(1,3) distribution
  # multiplied with shifted Weibull distribution function
  .decreasingBeta <- function(x, shift, shape, scale, support) {
    ((support - x) / support)^2 / beta(1, 3) * .shiftedPweibull(x, shift, shape, scale)
  }

  # Probability of X <= Y + shift with Weibull-distributed X and uniformly distributed Y on [0, maxAge],
  # where maxAge is the difference between initial time and the start of the vintage cohort.
  # for lower bounds, Y follows a rescaled Beta(1,3)-distribution, corresponding to a decreasing quadratic density
  # for upper bounds, Y follows a rescaled Beta(3,1)-distribution, corresponding to an increasing
  # quadratic density
  # The maximum age is capped at 25 years for beta functions.
  .probXleqY <- function(shift, shape, scale, maxAge, level = "central") {

    if (level == "lower") {
      func <- .decreasingBeta
      distrLength <- min(25, maxAge)
    } else if (level == "upper") {
      func <- .increasingBeta
      distrLength <- min(25, maxAge)
    } else if (level == "central") {
      func <- .shiftedPweibull
      distrLength <- maxAge
    } else {
      stop("Unexpected level: ", level)
    }

    # Numerically integrate over the density function
    res <- stats::integrate(
      func,
      lower = 0,
      upper = distrLength,
      shift = shift,
      shape = shape,
      scale = scale,
      support = distrLength
    )

    # Rescale result by the length of the support of the density
    1 / distrLength * res$value
  }



  # Set time periods -----------------------------------------------------------

  if (is.null(timePeriods)) {
    ttotIn <- if (isTRUE(standingLifeTime)) "tinit" else "ttot"
    timePeriods <- expandSets(ttotIn = ttotIn, ttotOut = "ttot", .m = m) %>%
      filter(.data$ttotIn <= .data$ttotOut)
  }

  params <- pivot_wider(params, names_from = "variable")

  commonCols <- intersect(names(timePeriods), names(params))
  share <- if (length(commonCols) > 0) {
    right_join(timePeriods, params, by = commonCols)
  } else {
    cross_join(timePeriods, params)
  }



  # Compute renovated share ----------------------------------------------------

  if (isTRUE(standingLifeTime)) {


    ## for standing stock ====

    # standing life time considers stock not flows -> no consideration of dt
    share <- share %>%
      cross_join(data.frame(level = as.character(levels))) %>%
      cross_join(vintages) %>%
      # Vintage cohorts start at 01.01.<from>, stock is taken at 31.12.<ttotIn>
      mutate(maxAge = .data$ttotIn - .data$from + 1,
             age = .data$ttotOut - .data$ttotIn) %>%
      filter(.data$maxAge > 0) %>% # Remove non-existing vintages
      select(-any_of(c("from", "to", "colour", "label")))

    # Only integrate unique cases for efficiency
    shareUniqueArgs <- share %>%
      select("age", "shape", "scale", "maxAge", "level") %>%
      unique()
    share <- shareUniqueArgs %>%
      group_by(across(everything())) %>%
      mutate(p0 = .probXleqY(
        0,
        .data$shape,
        .data$scale,
        .data$maxAge,
        .data$level
      ),
      p = .probXleqY(
        .data$age,
        .data$shape, .data$scale,
        .data$maxAge,
        .data$level
      )) %>%
      ungroup() %>%
      left_join(share, by = names(shareUniqueArgs))
  } else {


    # for renovations/construction ====

    # average past flow activity happened dt/2 before nominal time step ttotIn
    share <- share %>%
      left_join(readSymbol(m, "p_dt") %>%
                  rename(dt = "value"),
                by = c(ttotIn = "ttot")) %>%
      filter(!is.na(.data$dt)) %>%
      mutate(lt = .data$ttotOut - (.data$ttotIn - .data$dt / 2),
             p0 = 0,
             p  = pweibull(.data$lt, .data$shape, .data$scale))
  }

  share %>%
    mutate(value = (.data$p - .data$p0) / (1 - .data$p0),
           value = ifelse(.data$value > cutOffShare, 1, .data$value)) %>%
    select(-any_of(c("shape", "scale", "dt", "standingLt", "maxAge", "age", "lt", "p", "p0")))
}
