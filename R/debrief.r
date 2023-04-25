#' Debrief coder
#'
#' Debrief a coder when a job is finished.
#'
#' @param message A message in markdown to show when a coder finishes a job. The
#'   text is centered
#' @param link A url that will  be shown when the job is finished. If the url
#'   contains "{user_id}", this will be replaced with the Username
#' @param link_text the text shown on the button with the link
#' @param qr If TRUE, show QR code for creating a new job coder at the end, so
#'   coders can share the job with others
#'
#' @return A list containing debriefing information
#'
#' @export
debrief <- function(message, link = NULL, link_text = NULL, qr = FALSE) {
  l <- list(message = message)
  if (!is.null(link)) {
    if (is.null(link_text)) stop("If link is used, also give a link_text")
    l$link <- link
    l$link_text <- link_text
  }
  if (qr) l$qr <- TRUE
  l
}
