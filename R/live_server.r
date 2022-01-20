
#' Start annotation server (RStudio only)
#'
#' Starts a server and directs to the CSS annotator webclient.
#' You can either run the job in the current R session, or run it in another session.
#' If you run it in the current session (default), you can retrieve the annotations when you close (stop) the server.
#' If you run it in a different session, you can also retrieve the annotations while the server is still running.
#'
#' There are two ways to run in a different session. One is to simply open another R session, and run the server there.
#' The annotations are stored in a SQLite database, and can be retrieved from any R session with the gimme_annotations(db_file)
#' command that is printed when running this function. The other way is to use RStudio background jobs, which runs the
#' server in the background of the current session. To use this, set background = TRUE.
#'
#' @param codingjob   A codingjob, created with \code{\link{create_job}}
#' @param background  (RStudio only) If TRUE, start the server as an RStudio background job. This way you can
#'                    keep working in the current session
#' @param db_path     The path where the folder with coding job DBs is stored. Default is working directory.
#'                    If you're just playing around, tempdir() is pretty solid.
#' @param overwrite   You're not allowed to create two jobs with the same title (which also becomes the DB filename).
#'                    Or well, you're allowed to, but you have to say overwrite is TRUE so you can't blame us if you
#'                    accidentally delete any hard-earned annotations.
#' @param browse      If TRUE (default), automatically opens your default browser to start the annotation.
#'
#' @return
#' @export
#'
#' @examples
start_annotator <- function(codingjob, background=F, db_path=getwd(), overwrite=F, browse=T) {
  db_file = create_job_db(codingjob, path=db_path, overwrite=overwrite)
  Sys.setenv(ANNOTATION_DB = db_file)

  server_script = create_plumber_server_script(db_file)
  if (browse) view_annotator(in_browser = !background) ## if not background job, rstudio can't serve it

  if (background) {
    run_as_job(server_script)
  } else {
    run_in_current_session(db_file, server_script)
  }
  db_file
}



#' Create a
#'
#' @param codingjob
#' @param path
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
create_job_db <- function(codingjob, path=getwd(), overwrite=F) {
  folder = if (!is.null(path)) file.path(path, 'ccsAnnotatorJobs') else 'ccsAnnotatorJobs'
  if (!file.exists(folder)) dir.create(folder, recursive = T)
  filename = file.path(folder, paste0(codingjob$title, '.db'))

  if (file.exists(filename) && !overwrite) stop(sprintf('A codingjob with this name already exists.
                                                        If you are sure you want to overwrite is, set overwrite=T', folder))


  db = DBI::dbConnect(RSQLite::SQLite(), filename)
  db_write_codebook(db, codingjob$codebook)
  db_write_units(db, codingjob$units)
  db_get_codebook(db)
  db_get_unit(db, NA)
  DBI::dbDisconnect(db)
  return(filename)
}

run_as_job <- function(server_script) {
  pf = create_plumber_file(server_script)
  job = rstudioapi::jobRunScript(pf, workingDir = getwd())
  message('Visit annotation client at:\nhttps://ccs-amsterdam.github.io/ccs-annotator-client?rport=8000\nUse gimme_annotations() to fetch current annotations.')
  job
}

run_in_current_session <- function(db_file, server_script) {
  message(sprintf('Visit annotation client at:\nhttps://ccs-amsterdam.github.io/ccs-annotator-client?rport=8000\nRetreive annotations with:\ngimme_annotations("%s")', db_file))

  tryCatch({
    pr = plumber::pr(server_script)
    plumber::pr_run(pr, docs=F, port=8000)
  }, finally = {
    gimme_annotations(db_file)
  })
}



#' Get annotation for a given job DB
#'
#' @param db_file   If NULL (default) looks whether an an annotation server (see \code{\link{start_annotator}}) ran (or is running) in the current session.
#'                  If so, it retrieves the annotations from this server
#'                  annotations from this
#' @param only_done If TRUE (default) only retrieve annotations with the "DONE" status. If FALSE, also retrieve annotations with the "IN_PROGRESS" status
#'
#' @return
#' @export
#'
#' @examples
gimme_annotations <- function(db_file=NULL, only_done=TRUE) {
  if (is.null(db_file)) db_file = Sys.getenv('ANNOTATION_DB')
  if (is.null(db_file)) return(NULL)
  if_status = if (only_done) "DONE" else c("DONE","IN_PROGRESS")

  db = DBI::dbConnect(RSQLite::SQLite(), db_file)
  annotations = DBI::dbReadTable(db, 'annotations')
  DBI::dbDisconnect(db)
  annotations = annotations[annotations$json != '',]
  if (nrow(annotations) == 0) return(NULL)

  annotations = lapply(1:nrow(annotations), function(i) {
    a = jsonlite::fromJSON(annotations$json[i])
    if (!a$status %in% if_status) return(NULL)
    dplyr::bind_cols(unit_id = annotations$unit_id[i], dplyr::as_tibble(a$annotation))
  })
  dplyr::bind_rows(annotations)

}



create_plumber_server_script <- function(db_file) {
  server_file = tempfile(fileext = '.r')
  server_template = system.file("server_template.r", package="ccsAnnotator", mustWork=T)
  server_script = readChar(server_template, file.info(server_template)$size)
  server_script = gsub('DB_FILE', db_file, server_script)
  writeLines(server_script, server_file)
  server_file
}

create_plumber_file <- function(server_script) {
  start_server_file = tempfile(fileext = '.r')
  start_server_script = sprintf("library(ccsAnnotator)\nplumber::pr_run(plumber::pr('%s'), docs=F, port=8000)", server_script)
  writeLines(start_server_script, start_server_file)
  start_server_file
}
