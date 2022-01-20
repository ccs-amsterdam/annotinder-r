
#' Create a codebook for the CCS Annotator
#'
#' @param ...  Questions for "questions" mode or Variables for "annotate" mode. See \code{\link{codebook_variable}}
#'             Cannot combine "annotate" and "question", because these are very different annotation modes.
#'
#' @return A codebook object
#' @export
#'
#' @examples
#' variable = annotation_variable("sentiment", "Assign sentiment to words",
#'   codes = c(Negative='red', Neutral='grey', Positive='green'))
#'
#' create_codebook(variable)
create_codebook <- function(...) {
  l = list(...)

  has_variables = any(sapply(l, methods::is, 'codebookVariable'))
  has_questions = any(sapply(l, methods::is, 'codebookQuestion'))

  if (has_variables && has_questions) stop('Cannot have both "annotate" and "question" mode within the same codebook')
  if (!has_variables && !has_questions) stop('Need to provide at least one "annotate" or "question" argument')

  l = lapply(l, function(x) {
    x$codes = codes_df_to_list(x$codes)
    x
  })

  ##### TO ADD:
  ####### process branching

  if (has_variables) cb = list(type = jsonlite::unbox('annotate'), variables = l)
  if (has_questions) cb = list(type = jsonlite::unbox('questions'), questions = l)
  structure(cb, class=c('codebook', 'list'))
}

codes_df_to_list <- function(codes_df) {
  rownames(codes_df) = NULL  ## (otherwise can be come an object instead of an array in json)
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


