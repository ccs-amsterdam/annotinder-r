
## INITIALIZE

db_write_codebook <- function(db, codebook) {
  json = jsonlite::toJSON(codebook, auto_unbox = T)
  json_df = dplyr::tibble(json = json)
  DBI::dbWriteTable(db, 'codebook', json_df, overwrite=T)
}

db_write_units <- function(db, units) {
  json_list = sapply(units, jsonlite::toJSON, auto_unbox=T)
  index = (1:length(units)) - 1  ## start at zero to match js client indexing
  id = 1:length(units)
  external_id = sapply(units, function(x) as.character(x$id))
  json_df = dplyr::tibble(unit_index=index,
                          id=id,
                          external_id=external_id,
                          status="",
                          json=json_list) ## (db field name cannot be index)
  DBI::dbWriteTable(db, 'units', json_df, overwrite=T)
  DBI::dbExecute(db, 'CREATE INDEX unit_index_index ON units (unit_index)')
  DBI::dbExecute(db, 'CREATE INDEX id_index ON units (id)')

  db_create_annotations(db, units)
}

db_create_annotations <- function(db, units) {
  unit_id = 1:length(units)
  external_id = sapply(units, function(x) as.character(x$id))
  dummy_df = dplyr::tibble(unit_id=unit_id, external_id=external_id, json='')
  DBI::dbWriteTable(db, 'annotations', dummy_df, overwrite=T)
  DBI::dbExecute(db, 'CREATE INDEX unit_id_index ON annotations (unit_id)')
}


## GET



db_get_codebook <- function(db) {
  if (!DBI::dbExistsTable(db, 'codebook')) return(NULL)
  json_df = DBI::dbReadTable(db, 'codebook')
  codebook = jsonlite::fromJSON(json_df$json[1], simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F, flatten = F)
  structure(codebook, class=c('codebook','list'))
}

db_get_unit <- function(db, index) {
  if (is.na(index) || index < 0) {
    json_df = DBI::dbGetQuery(db, 'SELECT * FROM units WHERE status = "" OR status = "IN_PROGRESS" ORDER BY unit_index ASC LIMIT 1')
  } else {
    json_df = DBI::dbGetQuery(db, sprintf("SELECT * FROM units WHERE unit_index = %s", index))
  }

  if (nrow(json_df) != 1) {
    n_total = DBI::dbGetQuery(db, 'SELECT count(*) FROM units')
    return(list(index=as.numeric(n_total)))
  }

  unit = jsonlite::fromJSON(json_df$json[1], simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F, flatten = F)
  unit$index = json_df$unit_index
  unit$id = jsonlite::unbox(json_df$id)
  unit$status = jsonlite::unbox(json_df$status)

  ## THIS EXPOSES THE CONDITIONALS. This is not a problem if the r client is just for local coding and designing jobs,
  ## but if we want to also let people host a job we should implement the conditional checking in R
  unit$unit$conditionals = unit$conditionals

  ## add annotation if it had any
  annotation = db_get_annotation(db, unit$id)
  if (!is.null(annotation)) unit$annotation = annotation$annotation

  unit
}

db_get_annotation <- function(db, unit_id) {
  json_df = DBI::dbGetQuery(db, sprintf("SELECT * FROM annotations WHERE unit_id = '%s'", unit_id))
  if (nrow(json_df) != 1) return(NULL)
  if (json_df$json[1] == '') return(NULL)
  jsonlite::fromJSON(json_df$json[1], simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F, flatten = F)
}


db_get_progress <- function(db) {
  n_total = DBI::dbGetQuery(db, 'SELECT count(*) FROM units')
  n_coded = DBI::dbGetQuery(db,'SELECT count(*) FROM units WHERE status = "DONE" OR status = "SKIPPED"')
  l = list(n_coded = as.numeric(n_coded), n_total=as.numeric(n_total), seek_forwards=F, seek_backwards=T)
  l = lapply(l, jsonlite::unbox)
  l
}


## POST

db_insert_annotation <- function(db, unit_id, annotation) {
  annotation_json = jsonlite::toJSON(annotation, auto_unbox = T)
  safe_annotation = DBI::dbQuoteIdentifier(db, annotation_json)
  DBI::dbExecute(db, sprintf("UPDATE annotations SET json=%s WHERE unit_id = '%s'",
                     safe_annotation, unit_id))
  DBI::dbExecute(db, sprintf("UPDATE units SET status='%s' WHERE id='%s'",
                     annotation$status, unit_id))
}



