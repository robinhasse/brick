#' @importFrom gamstransfer Container

createCalibrationTarget <- function(path, outDir) {

  periods <- c(2000, 2005, 2010, 2015, 2020)

  dt <- data.frame(ttot = periods, dt = c(NA, diff(periods)))

  periodMap <- dt %>%
    filter(!is.na(.data$dt)) %>%
    group_by(period = .data$ttot) %>%
    reframe(ttot = seq(to = .data$period, length.out = .data$dt))

  m <- Container$new(file.path(path, "output.gdx"))

  vars <- c(
    stock = "v_stock",
    construction = "v_construction",
    renovation = "v_renovation",
    demolition = "v_demolition"
  )
  flows <- c("construction", "renovation", "demolition")

  v <- lapply(vars, readSymbol, x = m, selectArea = FALSE)

  v$stock <- v$stock %>%
    filter(.data$ttot %in% periods)

  v[flows] <- lapply(v[flows], function(flow) {
    flow %>%
      filter(.data$qty == "area") %>%
      right_join(periodMap, by = "ttot") %>%
      group_by(across(-all_of(c("ttot", "value")))) %>%
      summarise(value = mean(.data$value), .groups = "drop") %>%
      rename(ttot = "period")
  })

  v$renovation <- v$renovation %>%
    left_join(dt, by = "ttot") %>%
    mutate(ttotPrev = .data$ttot - .data$dt,
           untouched = .data$bs == "0" & .data$hs == "0") %>%
    left_join(v$stock,
              by = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc",
                     ttotPrev = "ttot"),
              suffix = c("", "Stock")) %>%
    group_by(across(all_of(c("qty", "vin", "reg", "loc", "typ", "inc", "ttot")))) %>%
    mutate(value = ifelse(.data$untouched,
                          .data$valueStock / .data$dt - sum(.data$value[!.data$untouched]),
                          .data$value)) %>%
    ungroup() %>%
    select(-"ttotPrev", -"dt", -"untouched", -"valueStock")

  v <- lapply(v, function(x) {
    x %>%
      group_by(across(-all_of(c("reg", "value")))) %>%
      summarise(value = signif(sum(.data$value), 4)) %>%
      mutate(reg = "EUR", .before = "loc")
  })

  header <- paste("* matching run:", path)
  lapply(names(v), function(var) {
    outFile <- file.path(outDir, paste0("f_", var, "CalibTarget.cs4r"))
    writeLines(header, outFile)
    write.table(v[[var]], outFile, append = TRUE, quote = FALSE,sep = ",",
                row.names = FALSE, col.names = FALSE)
  })

  return(invisible(v))
}
