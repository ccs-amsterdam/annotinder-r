

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
#' @param title     A character string, for the title of the codingjob
#' @param units     A codingjobUnits object, as created with \code{\link{create_units}}
#' @param codebook  A codebook object, as created with \code{\link{create_codebook}}. Can only be missing if jobsets with specific codebooks are provided (see jobsets argument)
#' @param annotations Optionally, create the job with imported annotations.
#'                    Should be a data.frame like returned by \code{\link{gimme_annotations}}, with the columns:
#'                    id, field, variable, value, offset and length. The "id" column should match the id column in the units
#'                    (based on the id argument in create_units). "field" should be a text field in units. "variable" and "value"
#'                    should match actual variables (or questions) and codes in the codebook. "offset" and "length" are the
#'                    character position offset and the length of the span annotation.
#' @param rules     A rules object, as created with one of the rules_* functions (e.g., \code{\link{rules_crowdcoding}}, \code{\link{rules_fixedset}}). If left empty, the 'crowdcoding' ruleset will be used.
#' @param jobsets   A list of jobsets, as created with \code{\link{jobset}}
#' @param pre       Optionally, a special unit or list of special units to show before the codingjob.
#' @param pre       Like pre, but for after the codingjob.
#'
#' @return   The id of the new codingjob on the server
#' @export
#'
#' @examples
upload_job <- function(title, units, codebook=NULL, annotations=NULL, rules = rules_fixedset(), jobsets=NULL, pre=NULL, post=NULL) {
  codingjob = create_job(title, units, codebook, annotations)

  pre = set_special_id(pre, 'pre')
  post = set_special_id(post, 'post')
  codingjob$units = c(pre, codingjob$units, post)

  codingjob$rules = rules
  if (!is.null(jobsets)) {
    if (anyDuplicated(sapply(jobsets, '[[', 'name')))
      stop('jobsets must have unique "name"')
    pre_ids = unlist(lapply(pre, '[[', 'id'))
    post_ids = unlist(lapply(post, '[[', 'id'))
    for (jobset in jobsets) {
      if (is.null(jobset$codebook)&& is.null(codingjob$codebook))
        stop('Either codingjob needs to have a codebook, or all jobsets must have a codebook')
      jobset$unit_set = c(pre_ids, jobset$unit_set, post_ids)
    }
  }

  cj_data = request('codingjob', post = T, json_data = jsonlite::toJSON(codingjob, auto_unbox = T))
  cj_data$id
}


set_special_id <- function(units, what) {
  if (is.null(units)) return(NULL)
  if (class(units) != 'list') units = list(units)
  for (i in 1:length(units)) {
    units[[i]]$id = paste0(what, ' ', i, ': ', units[[i]]$id)
  }
  units
}

