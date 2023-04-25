#' Get annotation for a given job DB
#'
#' @param db_file If NULL (default) looks whether an an annotation server (see
#'   \code{\link{start_annotator}}) ran (or is running) in the current session.
#'   If so, it retrieves the annotations from this server
#' @param only_done If TRUE (default) only retrieve annotations with the "DONE"
#'   status. If FALSE, also retrieve annotations with the "IN_PROGRESS" status
#'
#' @return  A tibble with annotations. The id column matches with the units (as
#'   specified in the id argument in create_units)
#' @export
#'
#' @examples
#' \dontrun{
#' # get annotation for most recent server run (or still running) in current session
#' gimme_annotations()
#'
#' # from a job database, as returned by create_job_db (or by start_annotator)
#' job_db <- "path/to/job.db"
#' gimme_annotations(job_db)
#' }
gimme_annotations <- function(db_file = NULL, only_done = FALSE) {
  if (is.null(db_file)) db_file <- Sys.getenv("ANNOTATION_DB")
  if (is.null(db_file)) {
    return(NULL)
  }

  db <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  annotations <- DBI::dbReadTable(db, "annotations")
  DBI::dbDisconnect(db)
  annotations <- annotations[annotations$json != "", ]
  if (nrow(annotations) == 0) {
    return(NULL)
  }

  annotations <- lapply(1:nrow(annotations), function(i) {
    a <- jsonlite::fromJSON(annotations$json[i])
    if (only_done && a$status != "DONE") {
      return(NULL)
    }
    dplyr::bind_cols(id = annotations$external_id[i], unit_status = a$status, dplyr::as_tibble(a$annotation))
  })
  dplyr::bind_rows(annotations)
}
