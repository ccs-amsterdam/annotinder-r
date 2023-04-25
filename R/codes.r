#' Create a codes data.frame
#'
#' @param ... codes as created with \code{\link{code}}. Can also be a list of
#'   codes
#'
#' @return a data.frame of codes
#' @export
#'
#' @examples
#' # plain list of codes
#' codes <- c("here", "some", "codes")
#'
#' # codes with extra arguments
#' codelist <- list(
#'   code("yes", color = "green"),
#'   code("no", color = "red")
#' )
#' bind_codes(codelist)
#'
#' # codes with children
#' codelist <- c(
#'   code(
#'     "birds",
#'     code("chicken"),
#'     code("dove")
#'   ),
#'   code(
#'     "mammals",
#'     code("dog"),
#'     code("bear")
#'   )
#' )
bind_codes <- function(...) {
  codelist <- list(...)
  if (!methods::is(codelist[[1]], "annotatorCode")) codelist <- codelist[[1]]
  df_list <- map_codes(codelist)
  d <- dplyr::bind_rows(df_list)
  if (anyDuplicated(d$code)) stop("Code names have to be unique")
  d
}


#' Create a code
#'
#' @param code   The name of the code
#' @param ...    Optionally, nested codes for children
#' @param color  Optionally, the color of the code
#' @param makes_irrelevant Optionally, a character vector of question names that become irrelevant if this code is the selected answer.
#'                         Can also be "REMAINING" to make all questions after the current one irrelevant.
#' @param required_for Optionally, a character vector of question names. If this code is not selected, all these questions become irrelevant.
#'                     Can also be "REMAINING" for all questions after the current.
#'
#' @return A annotatorCode object, to be used inside the \code{\link{codes}} function
#' @export
#'
#' @examples
#' code("yes", color = "green")
#' code("no", color = "red")
#'
#' # nested codes
#' code(
#'   "actor",
#'   code(
#'     "government",
#'     code("president"),
#'     code("vice-president")
#'   ),
#'   code("media"),
#'   code("society")
#' )
code <- function(code, ..., color = NULL, makes_irrelevant = NULL, required_for = NULL) {
  l <- list(code = code)
  children <- list(...)
  if (length(children) > 0) l$children <- children
  if (!is.null(color)) l$color <- color
  if (!is.null(makes_irrelevant)) l$makes_irrelevant <- makes_irrelevant
  if (!is.null(required_for)) l$required_for <- required_for

  if (!methods::is(l$code, "character")) stop("the code argument in the codes() function has to be character type")
  structure(l, class = c("annotatorCode", "list"))
}

map_codes <- function(codes, parent = NULL) {
  df <- data.frame(code = vector("character", length(codes)))
  if (!is.null(parent) & nrow(df) > 0) df$parent <- parent

  children <- list()

  for (i in seq_along(codes)) {
    code <- codes[[i]]

    if (methods::is(code, "character")) {
      df$code[i] <- code
    }

    if (methods::is(code, "annotatorCode")) {
      for (name in names(code)) {
        if (name == "children") next
        if (!name %in% colnames(df)) df[[name]] <- NA
        if (length(code[[name]]) > 1) {
          df[[name]][i] <- list(code[[name]])
        } else {
          df[[name]][i] <- code[[name]]
        }
      }
      if (!is.null(code$children)) {
        children[[""]] <- map_codes(code$children, parent = code$code)
      }
    }
  }
  c(list(df), children)
}
