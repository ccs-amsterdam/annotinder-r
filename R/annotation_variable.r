#' Create an annotation variable
#'
#' Creates an annotation variable that can be passed as an argument to \code{\link{create_codebook}}.
#'
#' @param name         The name/label of the variable. The name/label of the question. Recommended to keep short. Cannot contain a "."
#' @param instruction  A brief (think 1 or 2 sentences) instruction to the coder.
#' @param codes        The codes that the coder can choose from. Can be a character vector, named character vector or data.frame.
#'                     An unnamed character vector creates simple codes.
#'                     A named character vector uses the labels as colors, either as HEX or a name recognized by browsers (see \url{https://www.w3schools.com/colors/colors_names.asp}).
#'                     A data.frame must have a code column, and can use certain special columns (see details).
#'                     For most control, codes can be a list of 'code' objects created with \code{\link{code}}.
#' @param selection    The method for selecting codes. Can be "buttons" or "dropdown".
#'                     "buttons" shows all codes as a button, "dropdown" gives a dropdown menu with a search bar,
#'                     with in addition buttons for recently used codes
#' @param only_edit    If TRUE, coders can only edit imported annotations. You can import annotations in \code{\link{create_jobs}} with the annotations argument
#' @param only_imported If TRUE, codes can only use codes that were used at least once in a unit in the imported annotations.
#' @param multiple     If TRUE, coder can select multiple codes for a selected piece of text before closing the popup. Note that they can always select
#'                     multiple codes by opening the popup multiple times. The setting exists for cases where multiple codes for a selection are common (e.g. topics in a paragraph).
#'
#' @details
#' Using a data.frame for the codes argument gives more flexibility. This data.frame should have a "code" column, and can in addition have a "color" and "parent" column
#' The color should be a color name, either as HEX or a name recognized by browsers (see \url{https://www.w3schools.com/colors/colors_names.asp})
#' The parent column is only relevant if you have many codes and use selection="dropdown". The dropdown menu will then show the codes with parent names,
#' and parent names are included in the search string. A parent can be the name of another code, and parents can have parents,
#' thus creating trees (just make sure not to create cycles). Use case would for example be an ontology with actor -> government -> president, and issue -> economy -> taxes.
#'
#' @return A variable object, to be used within the \code{\link{create_codebook}} function
#' @export
#'
#' @examples
#' # simple variable with simple codes
#' codes = c("Economy","War","Health")
#' annotation_variable("topic", "Assign topics to words", codes=codes)
#'
#' # quick hand for setting colors
#' codes = c(Economy = 'blue', War = 'red', Health = 'green')
#' annotation_variable("topic", "Assign topics to words", codes=codes)
#'
#' # get codes from a data.frame
#' codes_df = data.frame(code  = c("negative","neutral","positive"),
#'                       color = c("red",     "grey",   "green"))
#' annotation_variable("sentiment", "Assign sentiment to words", codes_df)
#'
#' # codes data.frame with parents
#' codes_df = data.frame(
#'    parent = c('', 'actor', 'government', '', 'media', 'newspaper'),
#'    code = c('actor','government','president','media','newspaper','NYT'))
#' codes_df
#'
#' annotation_variable("actors", "Label actors. Use the most specific label available", codes_df)
annotation_variable <- function(name, instruction, codes=NULL, selection=c('buttons', 'dropdown'), only_edit=F, only_imported=F, multiple=F) {
  if (grepl('\\.', name)) stop('Variable name is not allowed to contain a "." symbol')

  selection = match.arg(selection)

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

  l$editMode = jsonlite::unbox(only_edit)
  l$onlyImported = jsonlite::unbox(only_imported)


  if (methods::is(l$codes, 'character')) {
    if (!is.null(names(l$codes))) {
      l$codes = data.frame(code = l$codes, color = names(l$codes))
    } else l$codes = data.frame(code = l$codes)
  }
  if (methods::is(l$codes, 'list')) {
    l$codes = bind_codes(codes)
  }
  if (!methods::is(l$codes, 'data.frame')) stop('The codes argument has to be a character vector, data.frame, or created with the codes() function')
  if (is.null(l$codes$code)) stop('The data.frame passed to the codes argument needs to have a column named "code"')
  structure(l, class=c('codebookVariable', 'list'))
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
