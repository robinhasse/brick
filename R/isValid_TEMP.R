.isValid <- function(x) {
  i = 0
  if (.isConfig(x)) {
    .isConfig2(x, i)
  } else if (.isCascade(x)) {
    .isCascade2(x, i)
  } else {
    stop("Neither cascade nor config")
  }
}

.isConfig2 <- function(x, i = 0) {
  valid <- .isConfig(x)
  if (!valid) {
    message(indent(i), "NO CONFIG!")
  }
  message(rep("  ", i), "config: ", x$title)
  nextConfigs <- attr(x, "nextConfigs")
  if (!is.null(nextConfigs)) {
    valid <- .isCascade2(nextConfigs, i + 1)
  }
  return(valid)
}

.isCascade2 <- function(x, i = 0) {
  valid <- .isCascade(x)
  if (!valid) {
    message(indent(i), "NO CASCADE!")
  }
  message(indent(i), "cascade")
  valid <- c()
  for (nm in names(x)) {
    valid <- c(valid, .isConfig2(x[[nm]], i + 1))
  }
  return(all(valid))
}

indent <- function(i) rep(" ", 2 * i)

