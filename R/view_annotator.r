
#' Open the annotator client
#'
#' Opens the annotator client. If you're running RStudio, it will try to use the Viewer.
#' Otherwise, it will open in your default webbrowser.
#'
#' @param in_browser Can be TRUE if you want to force opening in the default webbrowser
#'
#' @return Nothing
#' @export
#'
#' @examples
#' annotator_client()
annotator_client <- function(in_browser=FALSE) {
  react_build = system.file("ccs-annotator-client", package="ccsAnnotator", mustWork=T)

  viewer = getOption("viewer")

  if (is.null(viewer) || in_browser) {
    browser_url = file.path(react_build, 'build/index.html')
    utils::browseURL(browser_url)
  } else {
    ## RStudio can host it from an R temp file (see ?rstudioapi::viewer)
    tf = tempdir()
    url = file.path(tf, 'ccs-annotator-client/build/index.html')
    if (!file.exists(url)) file.copy(react_build, tf, recursive = T, overwrite = T)
    viewer(url)
  }
}
