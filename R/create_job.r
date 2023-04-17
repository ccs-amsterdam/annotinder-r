#' Create a CCS Annotator codingjob
#'
#' @param title A character string, for the title of the codingjob
#' @param units A codingjobUnits object, as created with
#'   \code{\link{create_units}}
#' @param codebook  A codebook object, as created with
#'   \code{\link{create_codebook}}. Can only be missing for jobs that will be
#'   uploaded to the AmCAT annotator backend, and for which jobsets with
#'   specific codebooks are specified.
#' @param pre A codingjobUnit or codingjobUnits list, created with
#'   \code{\link{create_unit}} or \code{\link{create_units}}
#' @param post Like pre, but shown after the codingjob.
#'
#' @return A codingjob object
#' @export
create_job <- function(title, units=NULL, codebook=NULL, pre=NULL, post=NULL) {
  if (!is.null(units) && !methods::is(units, 'codingjobUnits')) stop('units has to be a codingjobUnits list')

  codingjob = list(title=jsonlite::unbox(title),
                   codebook=codebook,
                   units=units)

  pre = prepare_position_unit(pre, 'pre')
  post = prepare_position_unit(post, 'post')
  codingjob$units = c(pre, codingjob$units, post)

  if (is.null(codebook)) {
    no_codebook = sapply(codingjob$units, function(unit) is.null(unit$unit$codebook))
    if (any(no_codebook)) stop('Job level codebook can only be missing if every unit in the job has its own codebook')
  }

  ids = sapply(codingjob$units, function(x) x$id)
  dupl = unique(ids[duplicated(ids)])
  if (length(dupl) > 0) {
    dupl_str = paste(head(dupl, 5), collapse=', ')
    if (length(dupl) > 5) dupl_str = paste0(dupl_str, ', (', lenght(dupl)-5, ' more)')
    sprintf('Job has duplicate unit ids: %s', dupl_str)
  }

  job = structure(codingjob, class=c('codingjob', 'list'))
  job$units = structure(job$units, class=c('codingjobUnits','list'))
  job
}

prepare_position_unit <- function(units, what) {
  if (is.null(units)) return(NULL)
  if (!is.null(units$id)) units = list(units)  ## if it has an $id, it's a single unit
  for (i in 1:length(units)) {
    units[[i]]$position = what
  }
  units
}
