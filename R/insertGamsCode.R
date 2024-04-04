#' Insert code into gams scripts
#'
#' Lines between marker comments are replaced by given lines. The respective
#' script file gets overwritten.
#'
#' Before
#' \preformatted{
#'   !! AUTOCODE.<section>
#'   <old code>
#'   !! AUTOCODE_end.<section>
#' }
#'
#' After
#' \preformatted{
#'   !! AUTOCODE.<section>
#'   <insert>
#'   !! AUTOCODE_end.<section>
#' }
#'
#' @param path character, description
#' @param script characer, name of gams script
#' @param section character, section name
#' @param insert character vector, code line that are inserted
#' @param keepComments logical, if TRUE, lines starting with \code{!!} are kept
#'   and the inserted lines follow below
#'
#' @author Robin Hasse
#'
#' @importFrom stringr str_escape

insertGamsCode <- function(path,
                           script,
                           section,
                           insert,
                           keepComments = TRUE) {

  # SETUP ----------------------------------------------------------------------

  marker <- "AUTOCODE"
  pcomment <- "^\\s*!!\\s*"
  endSuffix <- "_end"



  # FUNCTIONS ------------------------------------------------------------------

  findScript <- function(path, script) {
    file.path(path, "scripts", script)
  }


  findLine <- function(scriptTxt, pattern, onlyOne = TRUE) {
    line <- grep(pattern, scriptTxt)
    if (isTRUE(onlyOne)) {
      if (length(line) == 0) {
        stop("Can't find this pattern: ", pattern)
      } else if (length(line) > 1) {
        stop("There are multiple lines (", paste(line, collapse = ", "),
             ") with this pattern: ", pattern)
      }
    }
    return(line)
  }


  findSection <- function(scriptTxt, section) {
    pmarker <- paste0(pcomment, marker)
    pstart <- paste0(pmarker, "\\.", str_escape(section), "$")
    pend <- paste0(pmarker, endSuffix, "\\.", str_escape(section), "$")
    lines <- list(
      start = findLine(scriptTxt, pstart),
      end   = findLine(scriptTxt, pend)
    )
    if (lines$start >= lines$end) {
      stop("Start and end marker comments are in wrong order (lines ",
           paste(lines, collapse = " and "), ").")
    }
    return(lines)
  }


  findComments <- function(scriptTxt, sectionLines) {
    lines <- findLine(scriptTxt, pcomment, onlyOne = FALSE)
    lines[sectionLines$start < lines & lines < sectionLines$end]
  }


  insertLines <- function(scriptTxt, sectionLines, insert, keepComments) {
    linesKept <- if (isTRUE(keepComments)) {
      findComments(scriptTxt, sectionLines)
    } else {
      NULL
    }
    c(
      scriptTxt[1:sectionLines$start],
      scriptTxt[linesKept],
      insert,
      scriptTxt[sectionLines$end:length(scriptTxt)]
    )
  }



  # PROCESS --------------------------------------------------------------------

  scriptPath <- findScript(path, script)
  scriptTxt <- readLines(scriptPath)
  sectionLines <- findSection(scriptTxt, section)
  scriptTxt <- insertLines(scriptTxt, sectionLines, insert, keepComments)
  writeLines(scriptTxt, scriptPath)

}
