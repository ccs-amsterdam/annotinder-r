#' Create a CCS Annotator codingjob
#'
#' @param title     A character string, for the title of the codingjob
#' @param units     A codingjobUnits object, as created with \code{\link{create_units}}
#' @param codebook  A codebook object, as created with \code{\link{create_codebook}}. Can only be missing for jobs that will be uploaded
#'                  to the AmCAT annotator backend, and for which jobsets with specific codebooks are specified.
#' @param pre       A special unit or list of special question units created with \code{\link{create_question_unit}} to show before the codingjob.
#' @param post      Like pre, but shown after the codingjob.
#'
#' @return   A codingjob object
#' @export
#'
#' @examples
create_job <- function(title, units, codebook=NULL, pre=NULL, post=NULL) {
  if (!methods::is(units, 'createUnitsBundle')) stop('units has not been created with the create_units function')
  codingjob = list(title=jsonlite::unbox(title),
                    provenance=list(),
                    codebook=codebook,
                    units=prepare_units(units))

  ##### TO ADD:
  ####### check whether variables used in questions are present in units
  if (codebook$type == 'questions') {
    ## for every question, grep [.*], then setdiff with units$variables
  }

  pre = set_special_id(pre, 'pre')
  post = set_special_id(post, 'post')
  # if (!is.null(train)) {
  #   if (!methods::is(train, 'createGoldUnitsBundle')) stop('train units have not been created with the create_gold_units function')
  #   train = prepare_units(train)
  # }
  # if (!is.null(test)) {
  #   if (!methods::is(test, 'createGoldUnitsBundle')) stop('test units have not been created with the create_gold_units function')
  #   test = prepare_units(test)
  #   testmix = vector('list', length(codingjob$units) + length(test))
  #   unit_indices = sort(sample(1:length(testmix), length(codingjob$units), replace=F))
  #   testmix[unit_indices] = codingjob$units
  #   testmix[sapply(testmix, is.null)] = test
  #   codingjob$units = testmix
  # }
  codingjob$units = c(pre, codingjob$units, post)

  structure(codingjob, class=c('codingjob', 'list'))
}

set_special_id <- function(units, what) {
  if (is.null(units)) return(NULL)
  if (!is.null(units$id)) units = list(units)  ## if it has an $id, it's a single unit
  for (i in 1:length(units)) {
    units[[i]]$id = paste(what, i, sep='.')
  }
  units
}

