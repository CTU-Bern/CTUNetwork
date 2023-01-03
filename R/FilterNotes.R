#' Extracting text information encapsulated between html-like tags, e.g. Notes field
#'
#' @param Vector vector containing strings for which to extract information
#' @param BegCode vector containing strings with beginning tags, e.g. "<92.259156585>"
#' @param EndCode vector containing strings with ending tags, e.g. "</92.259156585>"
#'
#' @return vector
#' @importFrom stringr str_extract
#' @export
#'
#' @examples
#' Teststring <- <92.259156585> Hello world </92.259156585>
#' Data <- FilterNotes(Vector = Teststring, BegCode = "<92.259156585>", EndCode = "</92.259156585>")
#'
FilterNotes = function(Vector, BegCode, EndCode) {

  IdxNA = which(!is.na(Vector))
  # Regular expression online library:
  # https://regex101.com/

  # replace \n with <br/>
  Vector[IdxNA] = gsub(pattern = "\r\n", replacement = "<br/>", x = Vector[IdxNA])

  # This is the correct expression: (?<=<92\.259156585>)(.+)(?=<\/92\.259156585>)
  # https://stackoverflow.com/questions/72318707/regular-expression-issue-with-str-extract-all?noredirect=1#comment127761875_72318707
  Vector[IdxNA] = stringr::str_extract(Vector[IdxNA], paste0("(?s)(?<=",BegCode,")(.+)(?=",EndCode,")"))

  # Output
  return(Vector)
}
