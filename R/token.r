set_token <- function(token) {
  tf = token_file()
  saveRDS(token, tf)
  Sys.chmod(tf, mode='0600')
}

token_file <- function() {
  path = Sys.getenv("HOME")
  if (path == '') path = normalizePath('~')
  paste0(path, '/.annotinder.rds')
}

#' Delete backend API token
#'
#' Just in case you really want to.
#'
#' @return Nothing, just deletes the thing
#' @export
#'
#' @examples
#' delete_token()
delete_token <- function() {
  file.remove(token_file())
}
