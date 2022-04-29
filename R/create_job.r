#' Create a CCS Annotator codingjob
#'
#' @param title     A character string, for the title of the codingjob
#' @param units     A codingjobUnits object, as created with \code{\link{create_units}}
#' @param codebook  A codebook object, as created with \code{\link{create_codebook}}
#' @param rules     A rules object, as created with one of the rules_* functions (e.g., \code{\link{rules_crowdcoding}}, \code{\link{rules_fixedset}}). If left empty, the 'crowdcoding' ruleset will be used.
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
create_job <- function(title, units, codebook, rules = rules_crowdcoding(), annotations=NULL) {
  cj_package = list(title=jsonlite::unbox(title),
                    provenance=list(),
                    codebook=codebook,
                    units=prepare_units(units, annotations),
                    rules= rules)

  ##### TO ADD:
  ####### check whether variables used in questions are present in units
  if (codebook$type == 'questions') {
    ## for every question, grep [.*], then setdiff with units$variables
  }

  structure(cj_package, class=c('codingjob', 'list'))
}

## also create fast convenience options, like annotatinder

