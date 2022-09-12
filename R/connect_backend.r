#' Connect to an annotator backend API
#'
#' Connect to a backend API and request or refresh an authentication token. The typical mode of use is to provide the
#' 'host' (url) of the server and a 'username'. The password will then be requested in a separate prompt.
#'
#' @param host The hostname, e.g. http://localhost:5000
#' @param username An existing username on the host server
#' @param token An existing token to authenticate with. If given (and valid), no password has to be entered
#' @param .password Optionally, the password can be passed as an argument, but this is not recommended.
#'
#' @return Nothing. Stores API token in env
#' @export
backend_connect <- function(host, username, token=NULL, .password=NULL) {
  conn = structure(list(host = host, username = username, token = token),
                   class = c("annotatorBackendConnection",'list'))

  ## if active connection with same host and username, use existing token
  act_conn = conn_from_env()
  if (!is.null(act_conn)) {
    if (act_conn$host == host && act_conn$username == username) conn$token = act_conn$token
  }

  conn = login(conn, passwd = .password)

  conn_to_env(conn)
  invisible(conn)
}

login <- function(conn, passwd=NULL) {
  if (!is.null(conn$token)) {
    res = request(c('users','me','token'), conn=conn, read=F)
    if (!res$status_code == 200) {
      message('Token is not valid or expired. Please re-enter password')
      conn$token = NULL
    } else {
      conn$token = read_response(res)$token
    }
  }
  if (is.null(conn$token)) {
    conn$token = request_token(conn, passwd)
  }

  conn
}

conn_to_env <- function(conn) {
  if(!methods::is(conn, 'annotatorBackendConnection')) stop("conn is not an annotatorBackendConnection object")
  Sys.setenv(BACKEND_CONNECTION = jsonlite::toJSON(conn))
}

#' Get the current connection
#'
#' @return an annotatorBackendConnection connection object, created with \link{backend_connect}
#' @export
conn_from_env <- function(){
  backend_conn = Sys.getenv('BACKEND_CONNECTION')
  if (backend_conn == '') return(NULL)
  structure(jsonlite::fromJSON(backend_conn),
            class = c('annotatorBackendConnection','list'))
}

#' S3 print method for annotatorBackendConnection (API connection) objects
#'
#' @param x an annotatorBackendConnection connection object, created with \link{backend_connect}
#' @param ... not used
#'
#' @method print annotatorBackendConnection
#' @examples
#' \dontrun{
#' conn = backend_connect('http://localhost:8000')
#' conn
#' }
#' @export
print.annotatorBackendConnection <- function(x, ...){
  cat(sprintf('connection to server\nhost:\t%s\nuser:\t%s\n', x$host, x$username))
}

