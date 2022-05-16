#' Debrief coder
#'
#' Debrief a coder when a job is finished.
#'
#' @param message A message to show when a coder finishes a job.
#' @param link A url that will  be shown when the job is finished. If the url contains {user_id}, this will be replaced with the user ID (an email address or ID assigned via jobtoken)
#'
#' @return
#' @export
#'
#' @examples
debrief <- function(message, link=NULL) {
  l = list(message=message)
  if (!is.null(link)) l$link = link
  l
}
