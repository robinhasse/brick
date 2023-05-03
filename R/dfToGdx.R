#' Prepare data frame for GDX
#' 
#' Create an object that can be handled by [gdxrrw::wgdx()] to write the data
#' frame as a gams object into a gdx file.
#' 
#' @author Robin Hasse
#'
#' @param df data.frame with factor columns for gams dimensions and one value
#'   column.
#' @param name character, object name
#' @param type character, gams object type
#' @param uelCols character vector, column names of dimension columns, if NULL,
#'   all columns but the value column are considered.
#' @param valCol character, column name of the value column.
#' @param field character, for `valCol = "variable"`, it indicates which field
#'   is given as values.
#' @returns A list with a particular structure required by [gdxrrw::wgdx()].
#' @export
#'
#' @importFrom dplyr left_join
#'
dfToGdx <- function(df, name, type = "parameter", uelCols = NULL,
                        valcol = "value", field = "l") {

  # if uelCols are missing, all but the value column are considered
  if (is.null(uelCols)) {
    uelCols <- setdiff(colnames(df), valcol)
  }
  
  # unique dimension labels
  uels <- apply(df[, uelCols], 2, "unique", simplify = FALSE)
  uels <- sapply(uels, "sort", simplify = FALSE)
  
  # sort data frame by dimension labels
  df <- df[do.call("order", lapply(uelCols, function(col) df[[col]])), ]
  
  df[, uelCols] <- lapply(df[, uelCols], factor)
  
  # fill missing combinations of dimension labels with NA
  val <- do.call("expand.grid", uels)
  val <- left_join(val, df, uelCols)
  # sort val for array conversion
  val[] <- val[do.call("order", lapply(rev(uelCols), function(col) val[[col]])), ]
  # for (col in uelCols) {
  #   val[] <- val[order(val[[col]]), ]
  # }
  
  val <- array(val[[valcol]], lapply(uels, length), dimnames = uels)
  
  # create list for gams
  data <- list(name = name,
               val = val,
               type = type,
               uels = uels,
               dim = length(uelCols),
               form = "full")
  if (type == "variable") {
    data[["field"]] <- field
  }
  
  return(data)
}
