randomArray <- function(..., low = 1, high = 2) {
  x <- c(...)
  x <- array(NA, as.numeric(lapply(x, length)), x)
  if (!all(is.na(c(low, high)))) {
    x[] <- (high - low) * runif(length(x)) + low
  }
  x
}