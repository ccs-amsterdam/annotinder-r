
view_annotator <- function(in_browser=FALSE) {
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
