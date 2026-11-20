computeInstallationProfiles <- function(params, vintages, until) {
  cases <- params %>%
    pivot_wider(names_from = "variable") %>%
    select(-"region") %>%
    cross_join(vintages[c("from", "to")]) %>%
    unique() %>%
    filter(.data$from < until)

  .iterateProfil <- function(case, key) {
    periods <- seq(key$from, until)
    periodsConstr <- seq(key$from, key$to)

    installations <- c(0, 1)
    deinstallations <- c(0, 0) # only holds for long life times
    years <- periods[1] - c(1, 0)

    for (t in tail(periods, -1)) {
      # continuous installations
      fInstallations <- approxfun(years, installations, yleft = 0, rule = 2)

      # continuous deinstallations
      fInstTimesWB <- function(xIn, x) {
        fInstallations(xIn) * stats::pweibull(x - xIn,
                                            shape = key$shape,
                                            scale = key$scale)
      }

      # cummulated deinstallations
      fCumDeinstallations <- function(t) {
        res <- stats::integrate(fInstTimesWB,
                         lower = key$from - 1,
                         upper = t,
                         x = t,
                         stop.on.error = FALSE)$value
      }

      # first-time installations (during construction)
      inst <- if (t %in% periodsConstr) 1 else 0


      # replacements of previous installations that reached EOF within last year
      deinstallation <- fCumDeinstallations(t)
      repl <- deinstallation - tail(deinstallations, 1)

      installation <- inst + repl

      deinstallations <- c(deinstallations, deinstallation)
      installations <- c(installations, installation)
      years <- c(years, t)
    }



    data.frame(profile = tail(installations, -1),
               x = tail(years, -1))
  }

  cases %>%
    group_by(across(everything())) %>%
    dplyr::group_modify(.iterateProfil)
}
