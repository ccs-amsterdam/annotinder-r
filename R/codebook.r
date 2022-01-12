
create_codebook <- function(mode = c('annotate','questions'), ...) {
  mode = match.arg(mode)
  l = list(...)

  has_variables = any(sapply(l, methods::is, 'codebookVariable'))
  has_questions = any(sapply(l, methods::is, 'codebookQuestion'))

  if (mode == 'annotate') {
    if (!has_variables) stop('A codebook with "annotate" mode requires variables. See codebook_variables()')
    if (has_questions) stop('A codebook with "annotate" mode should not have questions, but variables. See codebook_variables')
  }
  if (mode == 'questions') {
    if (has_variables) stop('A codebook with "question" mode should not have variables, but questions. See codebook_questions')
    if (!has_questions) stop('A codebook with "question" mode requires questions. See codebook_questions()')
  }

  cb = list(type = mode, variables = l)
  structure(cb, class=c('codebook', 'list'))
}


create_codebook(
  mode = 'annotate',
  codebook_variable("topic", "Assign topics to words",
                     codes = c("Economy","War","Health"))
)


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


