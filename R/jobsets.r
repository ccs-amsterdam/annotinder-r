
#' Specify a jobset
#'
#' A simple codingjob has a single jobset, in which all coders have the same codebook and use the same set of units.
#' With this function you can instead create different jobsets, that you can pass in a list to the jobsets arguments in \code{\link{upload_job}}.
#'
#' @param name              String. The name of the jobset (e.g., experiment, control, set1, set2). Has to be unique across jobsets
#' @param codebook          A Codebook. If empty, the default codebook in the codingjob will be used.
#' @param ids               A character vector with unit IDs. If empty, all regular (i.e. not pre or post) units in the codingjob will be used
#' @param pre_ids           A character vector with unit IDS for pre units. If empty, all pre units will be used
#' @param post_ids           A character vector with unit IDS for post units. If empty, all post units will be used
#'
#' @details
#' To distribute coders evenly over the sets, each new coder will be assigned to the next set. When the last set is reached,
#' a new coder will be assigned to the first set and the cycle continues. In other words, a coders set is 'nth-coder modulus nr-of-sets'
#' If there are 2 sets, then coder 1 gets set 1, coder 2 gets set 2, coder 3 gets set 1, etc.
#'
#' @return A jobset
#' @export
#'
#' @examples
#' ## to create the rules argument in create_job
#' rules = rules_fixedset()
jobset <- function(name, codebook=NULL, ids=NULL, pre_ids=NULL, post_ids=NULL) {
  l = list(name = jsonlite::unbox(name))
  if (!is.null(codebook)) l[['codebook']] = codebook
  if (!is.null(ids)) l[['ids']] = ids
  if (!is.null(pre_ids)) l[['pre_ids']] = pre_ids
  if (!is.null(post_ids)) l[['post_ids']] = post_ids
  structure(l, class = c('jobset', 'list'))
}

