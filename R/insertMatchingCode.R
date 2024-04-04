insertMatchingCode <- function(path) {

  # READ REFERENCES GDX --------------------------------------------------------

  ## find file ====

  refGdx <- file.path(path, "references.gdx")

  if (!file.exists(refGdx)) {
    stop("Can'r find references.gdx in this path: ", path)
  }

  m <- Container$new(refGdx)


  ## read objects ====

  references <- readSymbol(m, "ref")
  refMaps <- grep(.refMapName(""), m$listSets(), value = TRUE)
  domains <- lapply(setNames(nm = refMaps), function(r) m$getSymbols(r)[[1]]$domain)



  # CODE GENERATION ------------------------------------------------------------

  ## declaration of refMaps ====

  insertGamsCode(path, "sets.gms", "declareRefMaps",
                 unlist(lapply(refMaps, function(refMap) {
                   domain <- paste(domains[[refMap]], collapse = ",")
                   paste0(refMap, "(", domain, ")")
                 })))


  ## loading of refMaps from gdx ====

  insertGamsCode(path, "sets.gms", "loadRefMaps",
                 paste("$load", refMaps))
}
