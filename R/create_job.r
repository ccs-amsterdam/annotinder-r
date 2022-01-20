#' Create a CCS Annotator codingjob
#'
#' @param title     A character string, for the title of the codingjob
#' @param units     A codingjobUnits object, as created with \code{\link{create_units}}
#' @param codebook  A codebook object, as created with \code{\link{create_codebook}}
#'
#' @return   A codingjob object
#' @export
#'
#' @examples
create_job <- function(title, units, codebook) {
  cj_package = list(title=jsonlite::unbox(title),
                    provenance=list(),
                    codebook=codebook,
                    units=prepare_units(units),
                    rules= list(ruleset = jsonlite::unbox('crowdcoding')))

  ##### TO ADD:
  ####### check whether variables used in questions are present in units
  if (codebook$type == 'questions') {
    ## for every question, grep [.*], then setdiff with units$variables
  }

  structure(cj_package, class=c('codingjob', 'list'))
}

## also create fast convenience options, like annotatinder

