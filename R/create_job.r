#' Create a CCS Annotator codingjob
#'
#' @param title     A character string, for the title of the codingjob
#' @param units     A codingjobUnits object, as created with \code{\link{create_units}}
#' @param codebook  A codebook object, as created with \code{\link{create_codebook}}. Can only be missing for jobs that will be uploaded
#'                  to the AmCAT annotator backend, and for which jobsets with specific codebooks are specified.
#' @param annotations Optionally, create the job with imported annotations.
#'                    Should be a data.frame like returned by \code{\link{gimme_annotations}}, with the columns:
#'                    id, field, variable, value, offset and length. The "id" column should match the id column in the units
#'                    (based on the id argument in create_units). "field" should be a text field in units. "variable" and "value"
#'                    should match actual variables (or questions) and codes in the codebook. "offset" and "length" are the
#'                    character position offset and the length of the span annotation.
#'
#' @return   A codingjob object
#' @export
#'
#' @examples
create_job <- function(title, units, codebook=NULL, annotations=NULL) {
  cj_package = list(title=jsonlite::unbox(title),
                    provenance=list(),
                    codebook=codebook,
                    units=prepare_units(units, annotations))

  ##### TO ADD:
  ####### check whether variables used in questions are present in units
  if (codebook$type == 'questions') {
    ## for every question, grep [.*], then setdiff with units$variables
  }

  structure(cj_package, class=c('codingjob', 'list'))
}

## also create fast convenience options, like annotatinder

