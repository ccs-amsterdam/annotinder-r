#' Create a variable for annotate mode in \code{\link{create_codebook}}
#'
#' @param name         The name/label of the variable
#' @param instruction  A brief (think 1 or 2 sentences) instruction to the coder.
#' @param codes        The codes that the coder can choose from. Can be a character vector.
#'                     For more detailed settings (custom colors, nesting) use the \code{\link{codes}}
#' @param selection    The method for selecting codes. Can be "buttons", "dropdown" or "dropdown_with_recent".
#'                     "buttons" shows all codes as a button, "dropdown" gives a dropdown menu with a search bar, and
#'                     "dropdown_with_recent" gives the dropdown with in addition buttons for recently used codes
#' @param onlyEdit     If TRUE, coders can only edit existing annotations. !! Note that this requires \code{\link{create_codebook}} to have
#'                     have imported annotations for this variable.
#' @param multiple     If TRUE, coder can select multiple codes for a selected piece of text before closing the popup. Note that they can always select
#'                     multiple codes by opening the popup multiple times. The setting exists for cases where multiple codes for a selection are common (e.g. topics in a paragraph).
#'
#' @return A variable object, to be used within the \code{\link{create_codebook}} function
#' @export
#'
#' @examples
#' # simple variable with simple codes
#' codebook_variable("topic", "Assign topics to words",
#'                   codes = c("Economy","War","Health"))
#'
#' # using codes and code functions for detailed codes
#' codebook_variable("sentiment", "Assign sentiment to words",
#' codes(
#'   code('negative', color='red'),
#'   code('neutral', color='grey'),
#'   code('positive', color='green')
#'   )
#' )
#'
#' # get codes from a data.frame
#' codes_df = data.frame(code  = c("negative","neutral","positive"),
#'                       color = c("red",     "grey",   "green"))
#' codebook_variable("sentiment", "Assign sentiment to words", codes_df)
codebook_variable <- function(name, instruction, codes, selection='buttons', onlyEdit=F, multiple=F) {
  l = as.list(environment())
  l[!sapply(l, is.null)]

  if (methods::is(l$codes, 'character')) l$codes = data.frame(code = l$codes)
  if (!methods::is(l$codes, 'data.frame')) stop('The codes argument has to be a character vector, data.frame, or created with the codes() function')
  if (is.null(l$codes$code)) stop('The data.frame passed to the codes argument needs to have a column named "code"')
  structure(l, class=c('codebookVariable', 'list'))
}

create_codebook(
  mode = 'annotate',
  codebook_variable("sentiment", "Select sentiment words and assign their polarity",
                    codes(
                      code('negative', color='red'),
                      code('neutral', color='grey'),
                      code('positive', color='green')
                    ))
)


#' S3 print method for codebookVariable objects
#'
#' @param x an codebookVariable object, created with \link{variable}
#' @param ... not used
#'
#' @method print codebookVariable
#' @examples
#' @export
print.codebookVariable <- function(x, ...){
  for (name in names(x)) {
    if (name == 'codes') next
    if (x[[name]] == F) next
    label = if (name == 'name') 'variable name' else name
    cat(sprintf('%s:\t%s\n', label, x[[name]]))
  }
  cat('\ncodes:\n')
  print(x$codes)
}

#' S3 summary method for codebookVariable objects
#'
#' @param x an codebookVariable object, created with \link{variable}
#' @param ... not used
#'
#' @method summary codebookVariable
#' @examples
#' @export
summary.codebookVariable <- function(x, ...){
  for (name in names(x)) {
    if (name == 'codes') next
    if (x[[name]] == F) next
    label = if (name == 'name') 'variable name' else name
    cat(sprintf('%s:\t%s\n', label, x[[name]]))
  }
}
