#' S3 print method for codingjobUnits objects
#'
#' @param x an codingjobUnits object, created with \link{create_units}
#' @param ... not used
#'
#' @method print codingjobUnits
#' @export
print.codingjobUnits <- function(x, ...){
  cat('List of', length(x), 'units\n')
}

#' S3 print method for codingjobUnit objects
#'
#' @param x an codingjobUnit object
#' @param ... not used
#'
#' @method print codingjobUnit
#' @export
print.codingjobUnit <- function(x, ...){
  cat(jsonlite::toJSON(x, pretty=T, auto_unbox=T))
}

combine_units <- function(...) {
  l = list(...)
  valid = sapply(l, function(x) methods::is(x, 'codingjobUnits') || methods::is(x, 'codingjobUnit'))
  if (!all(valid)) stop('codingjobUnits can only be combined (using the c function) with other codingjobUnits.')
  n = sapply(l, function(x) if (methods::is(x, 'codingjobUnits')) length(x) else 1)

  cju = structure(vector('list', sum(n)), class=c('codingjobUnits','list'))
  i = 1
  for (item in l) {
    if (methods::is(item, 'codingjobUnit')) {
      cju[[i]] = item
      i = i+1
    }
    if (methods::is(item, 'codingjobUnits')) {
      for (u in item) {
        cju[[i]] = u
        i = i+1
      }
    }
  }
  cju
}

#' S3 print method for combining codingjobUnits
#'
#' @param ... one or multiple codingjobUnit objects of codingjobUnits lists
#'
#' @method c codingjobUnit
#' @export
c.codingjobUnit <- function(...) combine_units(...)

#' S3 print method for combining codingjobUnits
#'
#' @param ... one or multiple codingjobUnit objects of codingjobUnits lists
#'
#' @method c codingjobUnits
#' @export
c.codingjobUnits <- function(...) combine_units(...)

