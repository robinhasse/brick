#' Check if stock balance is fulfilled
#'
#' Only works for sequential renovation right now.
#'
#' @param path character, directory of a brick run
#' @returns named list with deviations of each balance equation
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr %>% .data across all_of group_by summarise any_of left_join
#'   mutate inner_join select full_join rename semi_join
#' @importFrom tidyr replace_na
#' @export

checkStockBal <- function(path) {

  # FUNCTIONS ------------------------------------------------------------------

  .aggRen <- function(var, dim, dimr) {
    var <- var %>%
      mutate(dimrEffective = ifelse(.data[[dimr]] == "0",
                                    .data[[dim]],
                                    .data[[dimr]])) %>%
      group_by(across(-all_of(c(dim, dimr, "value")))) %>%
      summarise(value = sum(.data$value), .groups = "drop")
    names(var)[which(names(var) == "dimrEffective")] <- dim
    var
  }


  .addRenFrom <- function(v) {
    renFrom <- lapply(v[c("renovationBS", "renovationHS")], function(var) {
      var %>%
        group_by(across(-any_of(c("bsr", "hsr", "value")))) %>%
        summarise(value = sum(.data$value), .groups = "drop")
    })
    names(renFrom) <- paste0(names(renFrom), "_from")
    c(v, renFrom)
  }


  .addRenTo <- function(v) {
    renTo <- list(renovationBS_to = .aggRen(v$renovationBS, "bs", "bsr"),
                  renovationHS_to = .aggRen(v$renovationHS, "hs", "hsr"))
    c(v, renTo)
  }


  .addVinDim <- function(x, dtVin, dt) {
    x %>%
      left_join(dtVin, by = "ttot", relationship = "many-to-many") %>%
      left_join(dt, by = "ttot") %>%
      mutate(value = .data$value * .data$dtVin / .data$dt, .keep = "unused")
  }


  .shiftToPrev <- function(x, dt) {
    dt %>%
      mutate(tPrev = .data$ttot - .data$dt) %>%
      inner_join(x, by = c(tPrev = "ttot")) %>%
      select(-"dt", -"tPrev")
  }


  .joinVars <- function(x, y) {
    full_join(x, y, by = intersect(names(x), names(y)))
  }


  .calculate <- function(x, formul) {
    expr <- parse(text = formul)[[1]]
    eval(expr, envir = x, enclos = parent.frame())
  }


  .checkBal <- function(data, lhs, rhs, eps = 1E-4) {
    data$lhs <- .calculate(data, lhs)
    data$rhs <- .calculate(data, rhs)
    data$eqn <- data$lhs - data$rhs
    data[.isTRUE(abs(data$eqn) > eps), ]
  }



  # READ -----------------------------------------------------------------------

  gdx <- file.path(path, "output.gdx")
  m <- gamstransfer::Container$new(gdx)

  vars <- c("stock", "construction", "renovationBS", "renovationHS", "demolition")

  # further sets and parameters
  dt <- readSymbol(m, "p_dt") %>%
    select("ttot", dt = "value")
  dtVin <- readSymbol(m, "p_dtVin") %>%
    rename(dtVin = "value")
  periods <- readSymbol(m, "t")

  # read variables
  v <- lapply(setNames(nm = vars), function(var) {
    readSymbol(m, paste0("v_", var), stringAsFactor = FALSE)
  })




  # PREPARE --------------------------------------------------------------------

  # aggregate renovation
  v <- .addRenFrom(v)
  v <- .addRenTo(v)
  v$renovationBS <- NULL
  v$renovationHS <- NULL

  # assign construction to vintages
  v$construction <- .addVinDim(v$construction, dtVin, dt)

  # add previous stock
  v$stock_prev <- .shiftToPrev(v$stock, dt)

  # rename value columns
  v <- lapply(setNames(nm = names(v)), function(var) {
    x <- v[[var]]
    names(x)[names(x) == "value"] <- var
    x
  })

  data <- Reduce(.joinVars, v) %>%
    semi_join(periods, by = "ttot") %>%
    left_join(dt, by = "ttot") %>%
    replace_na(list(construction = 0))




  # CHECK ----------------------------------------------------------------------

  deviation <- list(
    stockBal1 = .checkBal(data,
                          lhs = "stock_prev + construction * dt",
                          rhs = "renovationBS_from * dt"),
    stockBal2 = .checkBal(data,
                          lhs = "renovationBS_to",
                          rhs = "renovationHS_from"),
    stockBal3 = .checkBal(data,
                          lhs = "renovationHS_to * dt",
                          rhs = "stock + demolition * dt")
  )



  return(deviation)
}
