request_token <- function(conn, passwd) {
  if (is.null(passwd)) passwd = getPass::getPass(paste('Enter password for user', conn$username))
  res = httr::POST(paste0(conn$host, '/users/me/token'), body = list(username=conn$username, password=passwd))
  if (!res$status_code == 200) {
    stop(paste("Could not get token for ", conn$username,"@", conn$host, " please check host, username and password"))
  }
  read_response(res)$token
}


#' call API with authentication and specified filters (GET or POST)
#'
#' Either provide the branch and param, or provide a full url with param included (ignoring param). If post is used, it is possible to provide the json_data directly (ignoring param)
#'
#' @param branch a character vector with the names of the API resources. For example, use c('projects','articlesets') for the url host/api/v4/projects/articlesets
#' @param param a named vector or list of parameters, e.g. c(project=2, articleset=3)
#' @param json_data For sending (post = TRUE) data, directly provide the json body. If used, the param argument is ignored.
#' @param post use HTTP POST instead of GET
#' @param post_options a list with options for HTTP POST (if post is TRUE)
#' @param read If TRUE, read the content from the response. Otherwise, return the response object
#' @param conn an API connection
#' @param ... parameters (param) in name-value pairs
#'
#' @return the response
#'
#' @export
request <- function(branch=NULL, param=list(), json_data=NULL, post=FALSE, post_options=list(), read=T, conn = conn_from_env(), ...) {
  if (is.null(conn)) stop('Not connected to an annotator backend. See backend_connect()')

  param = arrange_url_arguments(param, ...)
  url = paste(conn$host, paste(branch, collapse='/'), sep='/')
  if (!post) {
    res = httr::GET(url, get_headers(conn), query=param)
  } else {
    if (is.null(json_data)) json_data = jsonlite::toJSON(param, auto_unbox=T)
    res = httr::POST(url, body = json_data, httr::content_type_json(), httr::accept_json(), get_headers(conn))
  }

  if (read) read_response(res) else res
}

get_headers <- function(conn, param=list()) {
  if (!is.null(conn$token)) param[['Authorization']] = paste("Bearer", conn$token)
  do.call(httr::add_headers, args = param)
}

arrange_url_arguments <- function(l=NULL, ...) {
  l = c(list(...), l)                            ## add optional arguments
  l = l[!sapply(l, is.null)]                     ## drop NULL arguments
  if (is.null(l) | length(l) == 0) return(NULL)  ## return NULL if no arguments
  out = unlist(l, recursive = F, use.names = F)  ## expand arguments: list(x = c(1,2)) --> list(x=1, x=2)
  names(out) = rep(names(l), sapply(l, length))
  out = as.list(out)
  out[!duplicated(cbind(names(out), out))]       ## remove duplicated name-value pairs
}

load_rda <- function(bytes) {
  e = new.env()
  rconn = rawConnection(bytes)
  load(rconn, envir = e)
  close(rconn)
  as.list(e)
}

read_response <- function(res, only_2xx=T) {
  error_handling(res, only_2xx)
  parse_response(res)
}

parse_response <- function(res) {
  ct = res$headers$`content-type`
  if (is.null(ct)) return(NULL)
  if (grepl('application/x-r-rda', ct)) return(load_rda(res$content))
  if (grepl('application/json', ct)) return(jsonlite::fromJSON(rawToChar(res$content), simplifyVector=F))
  return(rawToChar(res$content))
}


error_handling <- function(res, only_2xx) {
  code_class = floor(res$status_code/100)
  cutoff_string <- function(x, n=500) if (nchar(x) > n) paste0(substr(x, 0, n), '...') else x

  if (code_class != 2 && only_2xx){
    res_msg = parse_response(res)

    res_msg_json = jsonlite::toJSON(res_msg)
    fn = tempfile()
    write(res_msg_json, file=fn)

    response = paste0("Full response written to ", fn, '\n', 'Use backend_error() to view')

    res_msg = res_msg[!names(res_msg) %in% c('error','status')]
    response_strings = paste(names(res_msg),
                             sapply(res_msg, function(x) cutoff_string(paste(x, collapse=' '))),
                             sep=':\t')

    msg = paste(response_strings, collapse='\n')
    Sys.setenv(BACKEND_ERROR = fn)
    stop("Unexpected Response Code ", res$status_code, "\n", msg, "\n\n", response, call. = F)
  }
}

#' View most recent AmCAT error
#'
#' @export
backend_error <- function() {
  fn = Sys.getenv('BACKEND_ERROR')
  txt = readLines(fn)
  json = jsonlite::toJSON(txt, pretty = T)
  jsonlite::prettify(json)
}
