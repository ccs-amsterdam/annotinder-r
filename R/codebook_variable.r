#' Create a variable for annotate mode in \code{\link{create_codebook}}
#'
#' codebook_variable creates a variable to use in a codebook. add_variable does the same, but can be used
#' to add a variable to a codebook in a pipe.
#'
#' @rdname codebook_variable
#' @param name         The name/label of the variable
#' @param instruction  A brief (think 1 or 2 sentences) instruction to the coder.
#' @param codes        The codes that the coder can choose from. Can be a character vector.
#'                     For more detailed settings (custom colors, nesting) use the \code{\link{codes}}
#'                     For convenient shorthand, can also be a named character vector where names are the codes and
#'                     values are colors (see example).
#' @param selection    The method for selecting codes. Can be "buttons" or "dropdown".
#'                     "buttons" shows all codes as a button, "dropdown" gives a dropdown menu with a search bar,
#'                     with in addition buttons for recently used codes
#' @param onlyEdit     If TRUE, coders can only edit existing annotations. !! Note that this requires \code{\link{create_codebook}} to have
#'                     have imported annotations for this variable.
#' @param multiple     If TRUE, coder can select multiple codes for a selected piece of text before closing the popup. Note that they can always select
#'                     multiple codes by opening the popup multiple times. The setting exists for cases where multiple codes for a selection are common (e.g. topics in a paragraph).
#' @param .codebook    The codebook to which the variable should be added (enables piping, see example)
#'
#' @aliases add_variable
#' @return A variable object, to be used within the \code{\link{create_codebook}} function
#' @export
#'
#' @examples
#' # simple variable with simple codes
#' codebook_variable("topic", "Assign topics to words",
#'    codes = c("Economy","War","Health"))
#'
#' # quick hand for setting colors
#' codebook_variable("topic", "Assign topics to words",
#'    codes = c(Economy = 'blue', War = 'red', Health = 'green'))
#'
#' # using codes and code functions for detailed codes
#' x = codebook_variable("sentiment", "Assign sentiment to words",
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
#'
#'
#' ## ADDING VARIABLES TO A CODEBOOK
#'
#' # directly with codebook_variable
#' create_codebook('annotate',
#'    codebook_variable("sentiment", "Assign sentiment to words", codes_df)
#' )
#'
#' # via a pipe with add_codebook_variable
#' create_codebook('annotate') |>
#'    add_variable("sentiment", "assign sentiment to words", codes_df)
codebook_variable <- function(name, instruction, codes=NULL, selection='buttons', onlyEdit=F, multiple=F) {
  a = as.list(environment())
  l = list(codes = codes)
  for (key in names(a)) {
    if (is.null(a[[key]])) next
    if (key == 'codes') next
    if (key == 'selection') {
      if (selection == 'buttons') {
        l[['searchBox']] = jsonlite::unbox(FALSE)
        l[['buttonMode']] = jsonlite::unbox('all')
      }
      if (selection == 'dropdown') {
        l[['searchBox']] = jsonlite::unbox(TRUE)
        l[['buttonMode']] = jsonlite::unbox('recent')
      }
      next
    }
    l[[key]] = jsonlite::unbox(a[[key]])
  }

  if (methods::is(l$codes, 'character')) {
    if (!is.null(names(l$codes))) {
      l$codes = data.frame(code = names(l$codes), color=l$codes)
    } else l$codes = data.frame(code = l$codes)
  }
  if (!methods::is(l$codes, 'data.frame')) stop('The codes argument has to be a character vector, data.frame, or created with the codes() function')
  if (is.null(l$codes$code)) stop('The data.frame passed to the codes argument needs to have a column named "code"')
  structure(l, class=c('codebookVariable', 'list'))
}

#' @rdname codebook_variable
#' @export
add_variable <- function(.codebook, name, instruction, codes=NULL, selection='buttons', onlyEdit=F, multiple=F) {
  cbv = codebook_variable(name=name, instruction=instruction, codes=codes, selection=selection, onlyEdit=onlyEdit, multiple=multiple)
  if (.codebook$type != 'annotate') stop('A codebook with "annotate" mode requires variables. See codebook_variables()')
  .codebook$variables = c(.codebook$variables, list(cbv))
  .codebook
}

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
