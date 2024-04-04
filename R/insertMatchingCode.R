insertMatchingCode <- function(path) {

  # READ REFERENCES GDX --------------------------------------------------------

  ## find file ====

  refGdx <- file.path(path, "references.gdx")

  if (!file.exists(refGdx)) {
    stop("Can't find references.gdx in this path: ", path)
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


  ## calculation of reference values ====

  missingScripts <- setdiff(paste0(references, ".gms"),
                            list.files(file.path(path, "matching")))
  if (length(missingScripts) > 0) {
    stop("The following gams scripts are missing in ", path, "/matching:\n  ",
         paste(missingScripts, collapse = "  \n"))
  }

  insertGamsCode(path, "equations.gms", "insertRefValEqn",
                 .insertBetween(paste0("$include matching/", references), "+"))
}
