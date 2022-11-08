
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
#' @param job_db      A codingjob database file, created with \code{\link{create_job_db}}
#' @param background  (RStudio only) If TRUE, start the server as an RStudio background job. This way you can
#'                    keep working in the current session
#' @param browse      If TRUE (default), automatically opens \code{\link{annotator_client}}
#'
#' @return The job_db (for piping convenience)
#' @export
#'
#' @examples
#' sentiment = annotation_variable('sentiment', 'assign sentiment to words',
#'      codes = c(red = 'Negative', grey = 'Neutral', green = 'Positive'))
#' codingjob = create_job('Sotu sentiment',
#'                        create_units(mini_sotu, id='id', set_text('text',text), meta=c('name','year')),
#'                        create_codebook(sentiment))
#'
#' job_db = create_job_db(codingjob)
#'
#' \dontrun{
#' start_annotator(job_db)
#' }
start_annotator <- function(job_db, background=F, browse=T, port=8000) {
  job_db = normalizePath(job_db)
  if (!file.exists(job_db)) stop(sprintf('The database file does not exist (%s)', job_db))

  Sys.setenv(ANNOTATION_DB = job_db)

  server_running = tryCatch(httr::GET('localhost:8000/users/me/login')$status == 200, error=function(e) FALSE)
  if (server_running) {
    ## if server already running, just replace the db file and restart client
    httr::POST('localhost:8000/db', body=jsonlite::toJSON(list(db_file=job_db), auto_unbox = T))
    if (browse) annotator_client(in_browser = !background) ## if not background job, rstudio can't serve it
    return(invisible(job_db))
  }

  server_script = create_plumber_server_script(job_db)
  if (browse) annotator_client(in_browser = !background) ## if not background job, rstudio can't serve it

  if (background) {
    run_as_job(server_script)
  } else {
    run_in_current_session(db_file, server_script)
  }
  invisible(job_db)
}


#' Create a codingjob database
#'
#' Creates an RSQlite database, that can be used in \code{\link{start_annotator}}
#'
#' @param codingjob   A codingjob, created with \code{\link{create_job}}
#' @param db_path     The path where the folder with coding job DBs is stored. Default is working directory.
#'                    If you don't want to store annotations beyond this session, use tempdir().
#' @param overwrite   You're not allowed to create two jobs with the same title (which also becomes the DB filename).
#'                    Or well, you're allowed to, but you have to say overwrite is TRUE so you can't blame us if you
#'                    accidentally delete any hard-earned annotations.
#'
#' @return The job database file, that can be used in gimme_annotation()
#' @export
#'
#' @examples
#' sentiment = annotation_variable('sentiment', 'assign sentiment to words',
#'      codes = c(red = 'Negative', grey = 'Neutral', green = 'Positive'))
#' codingjob = create_job('Sotu sentiment',
#'                        create_units(mini_sotu, id='id', set_text('text',text), meta=c('name','year')),
#'                        create_codebook(sentiment))
#'
#' job_db = create_job_db(codingjob)
create_job_db <- function(codingjob, path=getwd(), overwrite=F) {
  folder = if (!is.null(path)) file.path(path, 'annotinder_jobs') else 'annotinder_jobs'
  if (!file.exists(folder)) dir.create(folder, recursive = T)
  filename = file.path(folder, paste0(codingjob$title, '.db'))
  if (file.exists(filename) && !overwrite) stop(sprintf('A codingjob with this name already exists. If you are sure you want to overwrite is, set overwrite=T', folder))

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
  job = rstudioapi::jobRunScript(pf, name='AnnoTinder', workingDir = getwd())
  job
}

run_in_current_session <- function(db_file, server_script) {
  tryCatch({
    pr = plumber::pr(server_script)
    plumber::pr_run(pr, docs=F, port=8000)
  }, finally = "silence of the servers")
}

create_plumber_server_script <- function(db_file) {
  server_file = tempfile(fileext = '.r')
  server_template = system.file("server_template.r", package="annotinder", mustWork=T)
  server_script = readChar(server_template, file.info(server_template)$size)
  server_script = gsub('DB_FILE', db_file, server_script)
  writeLines(server_script, server_file)
  server_file
}


create_plumber_file <- function(server_script) {
  start_server_file = tempfile(fileext = '.r')
  start_server_script = sprintf("library(annotinder)\ntryCatch(plumber::pr_run(plumber::pr('%s'), docs=F, port=8000), error=function(e) NULL)", server_script)
  writeLines(start_server_script, start_server_file)
  start_server_file
}

