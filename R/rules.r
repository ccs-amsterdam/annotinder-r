#' CrowdCoding ruleset
#'
#' A ruleset that prioritizes coding the entire set as fast as possible using multiple coders.
#' When a coder requests a unit, the server will prioritize units that have been coded by least other coders.
#' After sorting by least coded units, the units are presented in the original order as uploaded to the server (i.e. the order used in \code{\link{create_units}}).
#' Note that you can use this to specify priorities, or to randomize the order.
#'
#' @param can_seek_backwards Boolean. Should the coder be allowed to go back to previous unit and edit them?
#' @param units_per_coder Number. The number of units presented to an individual crowd coder
#'
#' @return A rules object
#' @export
#'
#' @examples
#' ## to create the rules argument in create_job
#' rules = rules_crowdcoding()
rules_crowdcoding <- function(can_seek_backwards = TRUE, units_per_coder=NULL) {
  if (!class(can_seek_backwards) == 'logical') stop('can_seek_backwards has to be TRUE or FALSE')
  rules = list(ruleset = jsonlite::unbox('crowdcoding'),
               can_seek_backwards = jsonlite::unbox(can_seek_backwards))
  if (!is.null(units_per_coder)) {
    if (class(units_per_coder) != 'numeric') stop('units_per_coder has to be a number')
    rules[['units_per_coder']] = jsonlite::unbox(floor(units_per_coder))
  }
  structure(rules, class = c('annotatorRules', 'list'))
}

#' FixedSet ruleset
#'
#' A simple ruleset that present every coder with the exact same units, in the exact order of units (as created with \code{\link{create_units}})
#'
#' @param can_seek_backwards Boolean. Should the coder be allowed to go back to previous unit and edit them?
#'
#' @return A rules object
#' @export
#'
#' @examples
#' ## to create the rules argument in create_job
#' rules = rules_fixedset()
rules_fixedset <- function(can_seek_backwards = TRUE, can_seek_forwards = FALSE) {
  if (!class(can_seek_backwards) == 'logical') stop('can_seek_backwards has to be TRUE or FALSE')
  if (!class(can_seek_forwards) == 'logical') stop('can_seek_forwards has to be TRUE or FALSE')

  rules = list(ruleset = jsonlite::unbox('fixedset'),
               can_seek_backwards = jsonlite::unbox(can_seek_backwards),
               can_seek_forwards = jsonlite::unbox(can_seek_forwards))
  structure(rules, class = c('annotatorRules', 'list'))
}
