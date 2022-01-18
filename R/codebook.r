
#' Create a codebook for the CCS Annotator
#'
#' @param mode The type of annotation task. Currently supports "annotate" and "questions"
#' @param ...  Questions for "questions" mode or Variables for "annotate" mode. See \code{\link{codebook_variable}}
#'
#' @return A codebook object
#' @export
#'
#' @examples
#' create_codebook('annotate',
#' codebook_variable("sentiment", "Assign sentiment to words", codes(
#'   code('negative', color='red'),
#'   code('neutral', color='grey'),
#'   code('positive', color='green')
#' )))
create_codebook <- function(mode = c('annotate','questions'), ...) {
  mode = jsonlite::unbox(match.arg(mode))
  l = list(...)

  has_variables = any(sapply(l, methods::is, 'codebookVariable'))
  has_questions = any(sapply(l, methods::is, 'codebookQuestion'))

  if (mode == 'annotate') {
    if (has_questions) stop('A codebook with "annotate" mode should not have questions, but variables. See codebook_variables')
  }
  if (mode == 'questions') {
    if (has_variables) stop('A codebook with "question" mode should not have variables, but questions. See codebook_questions')
    if (!has_questions) stop('A codebook with "question" mode requires questions. See codebook_questions()')
  }

  l = lapply(l, function(x) {
    x$codes = codes_df_to_list(x$codes)
    x
  })

  if (mode == 'annotate') cb = list(type = jsonlite::unbox(mode), variables = l)
  if (mode == 'questions') cb = list(type = jsonlite::unbox(mode), questions = l)
  structure(cb, class=c('codebook', 'list'))
}

codes_df_to_list <- function(codes_df) {
  apply(codes_df, 1, function(x) {
    codes_l = as.list(x)
    lapply(codes_l, jsonlite::unbox)
  })
}


#' S3 print method for codebook objects
#'
#' @param x an codebook object, created with \link{variable}
#' @param ... not used
#'
#' @method print codebook
#' @examples
#' @export
print.codebook <- function(x, ...){
  cat("Codebook\nmode: ", x$type, '\n\n', sep = '')

  if (x$type == 'annotate') {
    for (v in x$variables) {
      if (is.null(v)) next
      cat(summary(v))
    }
  }
}


