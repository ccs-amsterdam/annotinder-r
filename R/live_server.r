



start_annotator <- function(codingjob, db_path=tempdir(), overwrite=F, browse=T) {
  db_file = create_job_db(codingjob, path=db_path, overwrite=overwrite)
  pf = create_plumber_file(db_file)

  rstudioapi::jobRunScript(pf, workingDir = getwd())
  message('Running annotation server. Visit at http://localhost:3000/ccs-annotator-client?rstudio=true')
  if (browse) browseURL('http://localhost:3000/ccs-annotator-client?rstudio=true')
}

create_job_db <- function(codingjob, path=tempdir(), overwrite=F) {
  folder = if (!is.null(path)) file.path(path, 'ccsAnnotatorJobs') else 'ccsAnnotatorJobs'
  if (!file.exists(folder)) dir.create(folder, recursive = T)
  filename = file.path(path, paste0(codingjob$title, '.db'))

  if (file.exists(filename) && !overwrite) stop(sprintf('A codingjob with this name already exists.
                                                        If you are sure you want to overwrite is, set overwrite=T', folder))

  db = DBI::dbConnect(RSQLite::SQLite(), filename)
  db_write_codebook(db, codingjob$codebook)
  db_write_units(db, codingjob$units)
  db_create_annotations(db)

  return(filename)
}


create_plumber_file <- function(db_file) {
  ## to use the db_file in the server (without importing the entire current env)
  ## directly add it to script and run script from temp file
  tmpdir = tempdir()

  server_file = file.path(tmpdir, 'server.r')
  server_template = system.file("server_template.r", package="ccsAnnotator", mustWork=T)
  server_script = readChar(server_template, file.info(server_template)$size)
  server_script = gsub('DB_FILE', db_file, server_script)
  writeLines(server_script, server_file)

  start_server_file = file.path(tmpdir, 'start_server.r')
  start_server_script = sprintf("library(ccsAnnotator)\nplumber::pr_run(plumber::pr('%s'), docs=F, port=8000)", server_file)
  writeLines(start_server_script, start_server_file)

  start_server_file
}
