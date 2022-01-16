db_write_codebook <- function(db, codebook) {
  json = jsonlite::toJSON(codebook)
  json_df = dplyr::tibble(json = json)
  DBI::dbWriteTable(db, 'codebook', json_df, overwrite=T)
}

db_read_codebook <- function(db, as_json=T) {
  if (!DBI::dbExistsTable(db, 'codebook')) return(NULL)
  json_df = DBI::dbReadTable(db, 'codebook')
  if (as_json) return(json_df$json[1])
  codebook = jsonlite::fromJSON(json_df$json[1])
  structure(codebook, class=c('codebook','list'))
}

db_write_units <- function(db, units) {
  u = prepare_units(units)
  json_list = sapply(u, jsonlite::toJSON)
  json_df = dplyr::tibble(id=1:length(json_list), json=json_list)
  DBI::dbWriteTable(db, 'units', json_df, overwrite=T)
  DBI::dbExecute(db, 'CREATE INDEX id_index ON units (id)')
}


db_get_unit <- function(db, index, as_json=T) {
  res = DBI::dbSendQuery(db, sprintf("SELECT * FROM units WHERE id = %s", index))
  json_df = dbFetch(res)
  if (nrow(json_df) != 1) return(NULL)
  if (as_json) return(json_df$json[1])
  jsonlite::fromJSON(json_df$json[1])
}

db_create_annotations <- function(db, annotations=NULL) {
  if (dbExistsTable(db, 'annotations')) dbRemoveTable(db, 'annotations')
  DBI::dbCreateTable(db, 'annotations', c(unit_id='INTEGER PRIMARY KEY', json='TEXT'))
  DBI::dbExecute(db, 'CREATE INDEX unit_id_index ON annotations (unit_id)')
}

db_insert_annotation <- function(db, index, annotation) {
  annotation = jsonlite::toJSON(annotation)
  safe_annotation = DBI::dbQuoteIdentifier(db, annotation)
  DBI::dbExecute(db, sprintf('INSERT OR REPLACE INTO annotations (unit_id, json) VALUES (%s, %s)',
                     index, safe_annotation))
}

db_get_annotation <- function(db, index, as_json=T) {
  res = DBI::dbSendQuery(db, sprintf("SELECT * FROM annotations WHERE unit_id = %s", index))
  json_df = dbFetch(res)
  if (nrow(json_df) != 1) return(NULL)
  if (as_json) return(json_df$json[1])
  jsonlite::fromJSON(json_df$json[1])
}
}
