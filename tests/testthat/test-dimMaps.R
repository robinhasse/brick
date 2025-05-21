test_that("first column of dimension mapping is called as dimension", {
  maps <- .getMapping("dim")
  for (dim in names(maps)) {
    expect_equal(names(maps[[dim]])[1], dim)
  }
})

test_that("dimension aggregation mappings are consistent with dimension mappings", {
  dimMaps <- .getMapping("dim")
  aggMaps <- .getMapping("agg")
  for (dim in names(aggMaps)) {
    expect_identical(head(names(aggMaps[[!!dim]]), 3),
                     c("dimGranularity", "dim", "dimAgg"))
    expect_identical(tail(names(aggMaps[[!!dim]]), -3),
                     !!tail(names(dimMaps[[dim]]), -1))
    expect_in(aggMaps[[!!dim]][["dim"]], dimMaps[[!!dim]][[!!dim]])
  }
})

test_that("there are consistent dimension aggregation mappings for the granularity mapping", {
  granularity.csv <- .getMapping("granularity.csv")
  aggMaps <- .getMapping("agg")
  dims <- setdiff(names(granularity.csv), "granularity")
  dimsWithAggMap <- names(aggMaps)
  for (dim in dims) {
    expect_in(!!dim, dimsWithAggMap)
    expect_in(granularity.csv[[!!dim]], c(NA, aggMaps[[!!dim]][["dimGranularity"]]))
  }
})
