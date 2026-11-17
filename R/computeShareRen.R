#' Compute the share of heating systems to be renovated
#'
#' Calculate share of buildings that need to be renovated or demolished between
#' given time steps assuming a Weibull distribution of the technology life time.
#'
#' If \code{standingLifeTime = TRUE}, a standing stock is considered.
#' For most technologies we assume a constant inflow between the start of the
#' respective vintage cohort and initial time.
#' For the more recent technology of heat pumps (\code{ehp1}), we assume that
#' the inflow decreases quadratically with the age of the system going back from
#' initial time to the start of the vintage cohort, but at most 25 years.
#' For the rather outdated technologies solids (\code{sobo}) and liquids (\code{libo}) boilers,
#' we assume that the inflow increases quadratically with the age of the system going back from
#' initial time to the start of the vintage cohort, but at most 25 (\code{sobo}) or 20 (\code{libo}) years.
#'
#' @param params data frame with parameters of the Weibull distribution
#' @param m Gams container with at least \code{ttotIn}, \code{ttotOut}, \code{tinit} and \code{p_dt}
#' @param cutOffShare numeric, renovation share at which we cut off the distribution function by jumping to 1
#' @param standingLifeTime logical, whether standing lifetime needs to be considered
#' @param timePeriods data frame with time periods for which the calculations should be done
#' @param vintages data frame with vintages and their starting and end year
#' @param ignoreVinAge logical, ignore vintage age in the assumption of when the
#'   initial stock was installed? This is non-physical but avoids oscillations.
#'
#' @returns Data frame of shares to be renovated in each time period
#'
#' @importFrom dplyr %>% .data across any_of cross_join everything filter group_by left_join mutate
#'   rename right_join select ungroup
#' @importFrom tidyr pivot_wider
#'
computeShareRen <- function(params, m,
                            cutOffShare = 1,
                            standingLifeTime = FALSE,
                            timePeriods = NULL,
                            vintages = NULL,
                            ignoreVinAge = TRUE) {


  tinit <- readSymbol(m, "tinit")


  # Functions ------------------------------------------------------------------

  # Weibull probability distribution function with shifted argument
  .shiftedPweibull <- function(x, shift, shape, scale, support = NULL, from = NULL, to = NULL) {
    stats::pweibull(x + shift, shape, scale)
  }

  # Density of rescaled Beta(3,1) distribution multiplied with shifted Weibull distribution function
  .increasingBeta <- function(x, shift, shape, scale, support, from = NULL, to = NULL) {
    (x / support)^2 / beta(3, 1) * .shiftedPweibull(x, shift, shape, scale)
  }

  # Density of rescaled Beta(1,3) distribution multiplied with shifted Weibull distribution function
  .decreasingBeta <- function(x, shift, shape, scale, support, from = NULL, to = NULL) {
    ((support - x) / support)^2 / beta(1, 3) * .shiftedPweibull(x, shift, shape, scale)
  }

  # identical replacement profile multiplied with shifted Weibull distribution function
  if (!ignoreVinAge && standingLifeTime) {
    .installationProfile <- computeInstallationProfiles(params, vintages, until = tinit)
    .identReplCycle <- function(x, shift, shape, scale, support, from, to, justProfile = FALSE) {
      .installationProfile(tinit - x, shape, scale, from = from, to = to) * if (justProfile) {
        1
      } else {
        .shiftedPweibull(x, shift, shape, scale)
      }
    }
  } else {
    .identReplCycle <- NULL
  }

  # Probability of X <= Y + shift with Weibull-distributed X and uniformly distributed Y on [0, maxAge],
  # where maxAge is the difference between initial time and the start of the vintage cohort.
  # If hs == "ehp1", Y follows a rescaled Beta(1,3)-distribution, corresponding to a decreasing quadratic density,
  # with the maximum age capped at 25 years.
  # If hs == "sobo" or hs == "libo", Y follows a rescaled Beta(3,1)-distribution, corresponding to an increasing
  # quadratic density, with the maximum age capped at 25 (sobo) and 20 (libo) years.
  .probXleqY <- function(shift, shape, scale, maxAge, from, to, constInstallation = TRUE) {

    distrLength <- maxAge

    # Set function to integrate over as the density of Y multiplied with the Weibull distribution function
    if (ignoreVinAge) {
      func <- .shiftedPweibull
      profileWeight <- distrLength
    } else {
      func <- .identReplCycle
      profileWeight <- stats::integrate(
        func,
        lower = 0,
        upper = distrLength,
        shift = shift,
        shape = shape,
        scale = scale,
        support = distrLength,
        from = from,
        to = to,
        justProfile = TRUE
      )$value
    }

    # Numerically integrate over the density function
    res <- stats::integrate(
      func,
      lower = 0,
      upper = distrLength,
      shift = shift,
      shape = shape,
      scale = scale,
      support = distrLength,
      from = from,
      to = to
    )

    # Rescale result by the length of the support of the density
    # TODO: this normalisation is not effective for identReplProfile as the
    # magnitude of installation profiles is uncontrolled.
    1 / profileWeight * res$value
  }



  # Set time periods -----------------------------------------------------------


  if (is.null(timePeriods)) {
    timePeriods <- expandSets(ttotIn = "ttot", ttotOut = "ttot", .m = m) %>%
      filter(.data$ttotIn <= .data$ttotOut)
    if (isTRUE(standingLifeTime)) {
      timePeriods <- filter(timePeriods, .data$ttotIn == tinit)
    }
  }

  params <- pivot_wider(params, names_from = "variable")

  commonCols <- intersect(names(timePeriods), names(params))
  share <- if (length(commonCols) > 0) {
    right_join(timePeriods, params, by = commonCols)
  } else {
    cross_join(timePeriods, params)
  }



  # Compute renovated share ----------------------------------------------------

  share <- if (isTRUE(standingLifeTime)) {


    ## for standing stock ====

    paramCols <- c("ttotIn", "ttotOut", "scale", "shape")
    # standing life time considers stock not flows -> no consideration of dt
    share %>%
      # remove duplicate parameters to save computation
      select(paramCols) %>%
      unique() %>%
      cross_join(vintages) %>%
      # assume an age long enough to cover almost all cases
      mutate(maxAge = pmin(stats::qweibull(0.995, shape = .data$shape, scale = .data$scale),
                           .data$ttotIn - .data$from + 1)) %>%
      filter(.data$maxAge > 0) %>% # Remove non-existing vintages
      select(-any_of(c("colour", "label"))) %>%
      group_by(across(everything())) %>%
      mutate(p0 = .probXleqY(
        0,
        .data$shape,
        .data$scale,
        .data$maxAge,
        from = .data$from,
        to = .data$to,
        constInstallation = ignoreVinAge
      ),
      p = .probXleqY(
        .data$ttotOut - .data$ttotIn,
        .data$shape, .data$scale,
        .data$maxAge,
        from = .data$from,
        to = .data$to,
        constInstallation = ignoreVinAge
      )) %>%
      ungroup() %>%
      # blow up to all (duplicate) cases again
      full_join(share, by = paramCols, relationship = "many-to-many")
  } else {


    # for renovations/construction ====

    # Weibull for age = ttotOut - ttotIn
    # assuming ttotIn is equally distributed across dt until nominal ttotIn
    .pWeibullSegment <- function(ttotIn, ttotOut, dt, shape, scale) {
      lower = ttotOut - ttotIn
      upper = ttotOut - (ttotIn - dt)
      res <- stats::integrate(f = pweibull, lower = lower, upper = upper,
                              shape = shape, scale = scale)
      res$value / dt
    }

    # average past flow activity happened equally distributed across the dt until ttotIn
    share %>%
      left_join(readSymbol(m, "p_dt") %>%
                  rename(dt = "value"),
                by = c(ttotIn = "ttot")) %>%
      filter(!is.na(.data$dt)) %>%
      mutate(p0 = 0,
             p = unlist(Map(.pWeibullSegment,
                            ttotIn = .data$ttotIn,
                            ttotOut = .data$ttotOut,
                            dt = .data$dt,
                            shape = .data$shape,
                            scale = .data$scale)))
  }

  share %>%
    mutate(value = (.data$p - .data$p0) / (1 - .data$p0),
           value = ifelse(.data$value > cutOffShare, 1, .data$value)) %>%
    select(-any_of(c("shape", "scale", "dt", "standingLt", "maxAge", "lt", "p", "p0")))
}
