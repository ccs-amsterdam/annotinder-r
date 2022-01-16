#' Create a codes data.frame
#'
#' @param ... unnamed arguments, each one a code. Can either be a character like "yes",
#'            or a more detailed code created with the \code{\link{code}} function.
#'
#' @return
#' @export
#'
#' @examples
#' # plain list of codes
#' codes('here','some','codes')
#'
#' # codes with extra arguments
#' codes(
#'    code('yes', color='green'),
#'    code('no', color='red')
#' )
#'
#' # codes with children
#' codes(
#'   code('birds',
#'      code('chicken'),
#'      code('dove')
#'    ),
#'   code('mammals',
#'      code('dog'),
#'      code('bear')
#'    )
#' )
codes <- function(...) {
  df_list = map_codes(list(...))
  d = dplyr::bind_rows(df_list)
  if (anyDuplicated(d$code)) stop('Code names have to be unique')
  d
}

#' Create a code
#'
#' @param code   The name of the code
#' @param ...    Optionally, nested codes for children
#' @param color  Optionally, the color of the code
#'
#' @return A annotatorCode object, to be used inside the \code{\link{codes}} function
#' @export
#'
#' @examples
#' code('yes', color='green')
#' code('no', color='red')
code <- function(code, ..., color=NULL) {
  l = list(code = code)
  children = list(...)
  if (length(children) > 0) l$children = children
  if (!is.null(color)) l$color = color

  if (!methods::is(l$code, 'character')) stop('the code argument in the codes() function has to be character type')
  structure(l, class=c("annotatorCode",'list'))
}

map_codes <- function(codes, parent=NULL) {
  df = data.frame(code = vector('character', length(codes)))
  print(names(codes))
  if (!is.null(parent) & nrow(df) > 0) df$parent = parent

  children = list()

  for (i in seq_along(codes)) {
    code = codes[[i]]

    if (methods::is(code, 'character')) {
      df$code[i] = code
    }

    if (methods::is(code, 'annotatorCode')) {

      for (name in names(code)) {
        if (name == 'children') next
        if (!name %in% colnames(df)) df[[name]] = jsonlite::unbox(NA)
        df[[name]][i] = jsonlite::unbox(code[[name]])
      }
      if (!is.null(code$children)) {
        children[['']] = map_codes(code$children, parent=code$code)
      }
    }
  }
  c(list(df), children)
}
