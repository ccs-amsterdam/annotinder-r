#' Create a CCS Annotator codingjob
#'
#' @param title     A character string, for the title of the codingjob
#' @param units     A codingjobUnits object, as created with \code{\link{create_units}}
#' @param codebook  A codebook object, as created with \code{\link{create_codebook}}. Can only be missing for jobs that will be uploaded
#'                  to the AmCAT annotator backend, and for which jobsets with specific codebooks are specified.
#' @param pre       A special unit or list of special question units created with \code{\link{create_question_unit}} to show before the codingjob.
#' @param post      Like pre, but shown after the codingjob.
#' @param training  If there are gold units, you can use some of them for training the coders at the start of the session. These will be shown
#'                  at the start of the session (after pre).
#' @param test      Mix gold units with regular units to test whether coders are performing well. For local jobs in R, the gold units
#'                  are just added randomly. For jobs uploaded to the AmCAT annotator backend, different strategies can be set with the rules.
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
create_job <- function(title, units, codebook=NULL, pre=NULL, post=NULL, training=0, annotations=NULL) {
  if (!methods::is(units, 'createUnitsBundle')) stop('units has not been created with the create_units function')
  cj_package = list(title=jsonlite::unbox(title),
                    provenance=list(),
                    codebook=codebook,
                    units=prepare_units(units, annotations))

  ##### TO ADD:
  ####### check whether variables used in questions are present in units
  if (codebook$type == 'questions') {
    ## for every question, grep [.*], then setdiff with units$variables
  }

  pre = set_special_id(pre, 'pre')
  post = set_special_id(post, 'post')
  if (!is.null(train)) {
    if (!methods::is(train, 'createGoldUnitsBundle')) stop('train units have not been created with the create_gold_units function')
    train = prepare_units(train)
  }
  if (!is.null(test)) {
    if (!methods::is(test, 'createGoldUnitsBundle')) stop('test units have not been created with the create_gold_units function')
    test = prepare_units(test)
    testmix = vector('list', length(codingjob$units) + length(test))
    unit_indices = sort(sample(1:length(testmix), length(codingjob$units), replace=F))
    testmix[unit_indices] = codingjob$units
    testmix[sapply(testmix, is.null)] = test
    codingjob$units = testmix
  }
  codingjob$units = c(pre, train, codingjob$units, post)

  structure(cj_package, class=c('codingjob', 'list'))
}

set_special_id <- function(units, what) {
  if (is.null(units)) return(NULL)
  if (!is.null(units$id)) units = list(units)  ## if it has an $id, it's a single unit
  for (i in 1:length(units)) {
    units[[i]]$id = paste(what, i, sep='.')
  }
  units
}

