db_write_codebook <- function(db, codebook) {
  json = jsonlite::toJSON(codebook)
  json_df = dplyr::tibble(json = json)
  DBI::dbWriteTable(db, 'codebook', json_df, overwrite=T)
}

db_read_codebook <- function(db) {
  if (!DBI::dbExistsTable(db, 'codebook')) return(NULL)
  json_df = DBI::dbReadTable(db, 'codebook')
  codebook = jsonlite::fromJSON(json_df$json[1])
  structure(codebook, class=c('codebook','list'))
}

db_write_units <- function(db, units) {
  json_list = sapply(units, jsonlite::toJSON)
  id = (1:length(units)) - 1   ## start at zero, so ids match indices from 0
  json_df = dplyr::tibble(id=id, status="", json=json_list)
  DBI::dbWriteTable(db, 'units', json_df, overwrite=T)
  DBI::dbExecute(db, 'CREATE INDEX id_index ON units (id)')
}


db_get_unit <- function(db, index) {

  if (is.na(index)) {
    res = DBI::dbSendQuery(db, 'SELECT * FROM units WHERE status = "" OR status = "IN_PROGRESS" ORDER BY id ASC LIMIT 1')
  } else {
    res = DBI::dbSendQuery(db, sprintf("SELECT * FROM units WHERE id = %s", index))
  }

  json_df = DBI::dbFetch(res)
  DBI::dbClearResult(res)
  if (nrow(json_df) != 1) return(NULL)

  unit = jsonlite::fromJSON(json_df$json[1])
  unit$id = jsonlite::unbox(json_df$id)
  unit$status = jsonlite::unbox(json_df$status)

  ## add annotation if it had any
  annotation = db_get_annotation(db, unit$id)
  if (!is.null(annotation)) unit$annotation = annotation$annotation

  unit
}


db_create_annotations <- function(db, annotations=NULL) {
  if (DBI::dbExistsTable(db, 'annotations')) DBI::dbRemoveTable(db, 'annotations')
  DBI::dbCreateTable(db, 'annotations', c(unit_id='INTEGER PRIMARY KEY', json='TEXT'))
  DBI::dbExecute(db, 'CREATE INDEX unit_id_index ON annotations (unit_id)')
}

db_insert_annotation <- function(db, unit_id, annotation) {
  annotation_json = jsonlite::toJSON(annotation)
  safe_annotation = DBI::dbQuoteIdentifier(db, annotation_json)
  DBI::dbExecute(db, sprintf('INSERT OR REPLACE INTO annotations (unit_id, json) VALUES (%s, %s)',
                     unit_id, safe_annotation))
  DBI::dbExecute(db, sprintf('UPDATE units SET status="%s" WHERE id=%s',
                     annotation$status, unit_id))
}


db_get_annotation <- function(db, unit_id) {
  res = DBI::dbSendQuery(db, sprintf("SELECT * FROM annotations WHERE unit_id = %s", unit_id))
  json_df = DBI::dbFetch(res)
  DBI::dbClearResult(res)
  if (nrow(json_df) != 1) return(NULL)
  jsonlite::fromJSON(json_df$json[1])
}


db_get_progress <- function(db) {
  res = DBI::dbSendQuery(db, 'SELECT count(*) FROM units')
  n_total = as.numeric(DBI::dbFetch(res))
  DBI::dbClearResult(res)
  res = DBI::dbSendQuery(db,'SELECT count(*) FROM units WHERE status = "DONE" OR status = "SKIPPED"')
  n_coded = as.numeric(DBI::dbFetch(res))
  DBI::dbClearResult(res)
  l = list(n_coded = n_coded, n_total=n_total, seek_forwards=F, seek_backwards=T)
  l = lapply(l, jsonlite::unbox)
  l
}
