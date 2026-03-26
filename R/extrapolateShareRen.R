#' Extrapolate share of initial heating system to replace
#'
#' Extrapolation to future time steps such that the matching results can be used
#' for temporal scopes that go beyond the matching scope.
#'
#' @param path character, directory of matching folder
#' @returns data frame with \code{v_shareRenHSinit} extrapolated to future time
#'   steps
#'
#' @author Robin Hasse
#'
#'@importFrom dplyr %>% .data mutate full_join group_by across all_of select
#'  ungroup
#'@importFrom tidyr pivot_wider

extrapolateShareRen <- function(path) {


  # READ -----------------------------------------------------------------------

  gdxOutput <- file.path(path, "output.gdx")
  gdxInput <- file.path(path, "input.gdx")

  v_shareRenHSinit <- readSymbol(gdxOutput, "v_shareRenHSinit")
  p_shareRenHSfullinit <- readSymbol(gdxInput, "p_shareRenHSfullinit") %>%
    mutate(ttot = as.numeric(as.character(.data$ttotOut)), .keep = "unused")



  # CALCULATE ------------------------------------------------------------------

  dims <- setdiff(names(v_shareRenHSinit), "value")
  periods <- unique(p_shareRenHSfullinit$ttot)

  data <- p_shareRenHSfullinit %>%
    pivot_wider(names_from = "level") %>%
    full_join(interpolate_missing_periods(v_shareRenHSinit, ttot = periods), by = dims) %>%
    group_by(across(-all_of(c("ttot", "value", "lower", "central", "upper")))) %>%
    mutate(lastPeriod = max(.data$ttot[!is.na(.data$value)]),
           lastValue = .data$value[.data$ttot == .data$lastPeriod],
           bound = ifelse(.data$lastValue >= .data$central[.data$ttot == .data$lastPeriod],
                          .data$upper, .data$lower),
           w = ((.data$value - .data$central) / (.data$bound - .data$central))[.data$ttot == .data$lastPeriod]) %>%
    ungroup() %>%
    mutate(value = ifelse(is.na(.data$value),
                          .data$w * .data$bound + (1 - .data$w) * .data$central,
                          .data$value)) %>%
    select(all_of(c(dims, "value"))) %>%
    mutate(level = "matched", .before = 1)



  # OUTPUT ---------------------------------------------------------------------

  write.csv(data, file.path(path, "f_shareRenHSinit.csv"), row.names = FALSE)

  return(invisible(data))
}
