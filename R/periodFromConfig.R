#' get Period from config
#'
#' extract different periods from a given BRICK config
#'
#' @author Robin Hasse
#'
#' @param config named list with run configuration
#' @param which type of period(s)
#' @returns numeric vector with periods

periodFromConfig <- function(config, which) {

  # read
  startyear <- config[["startyear"]]
  ttot <- sort(unique(config[["periods"]]))

  if (startyear <= min(ttot)) {
    stop("startyear cannot be equal or before the first period. ",
         "There has to be at least one historic period.")
  }

  # derive
  t <- ttot[which(ttot >= startyear)]
  tall <- min(ttot):max(ttot)
  thist <- setdiff(ttot, t)
  tinit <- min(ttot)
  t0 <- min(t)

  # return
  switch(which,
    total = ttot,
    all = tall,
    startyear = startyear,
    model = t,
    historic = thist,
    initial = tinit,
    reference = t0,
    stop("unknown type of period: ", which)
  )
}
