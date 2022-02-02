

#' Get a list of codingjobs
#'
#' Get the codingjobs for the current host and user, logged in with \code{\link{backend_connect}}
#'
#' @return  A data.frame with the codingjobs
#' @export
#'
#' @examples
list_jobs <- function() {
  request('codingjobs')
}


get_job <- function(job_id, annotations=T) {
  cj = request(c('codingjob', job_id), annotations=T)

  annotations = lapply(cj$annotations, function(a) {
    d = dplyr::bind_rows(a$annotation)
    dplyr::bind_cols(a[names(a) != 'annotation'], d)
  })
  d = dplyr::bind_rows(annotations)

  units = structure(cj$units, class=c('codingjobUnits', 'list'))
  codebook = structure(cj$codebook, class=c('codebook', 'list'))
}

#' Upload a CCS Annotator codingjob to a server
#'
#' @param codingjob A codingjob created with \code{\link{create_job}}
#'
#' @return   The id of the new codingjob on the server
#' @export
#'
#' @examples
upload_job <- function(codingjob) {
  cj_data = request('codingjob', post = T, json_data = jsonlite::toJSON(codingjob, auto_unbox = T))
  cj_data$id
}


