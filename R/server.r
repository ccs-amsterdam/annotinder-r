
function() {

library(plumber)
sentiment_codes = c(Negative = 'red', Neutral = 'grey', Positive = 'green')

codebook = create_codebook('annotate') |>
  add_variable("sentiment", "assign sentiment to words", sentiment_codes)

units = create_units(mini_sotu, id = 'id', text='text', meta=c('name','year'))

units_df = units$df
codingjob = create_job('Sotu sentiment', units, codebook)


token = "supersecrettoken"

s = plumber::pr('R/preview_server.r',)
plumber::pr_run(s, docs=F, port=8000)




filename = 'test.db'
db = DBI::dbConnect(RSQLite::SQLite(), filename)
db_write_codebook(db, codebook)
db_read_codebook(db)


db_listclass_write(db, 'codebook', codebook)




DBI::dbWriteTable(db, "codebook", dplyr::tibble(json = jsonlite::toJSON(codebook)))






dplyr::copy_to(con, name='codebook', dplyr::tibble(json=jsonlite::toJSON(codebook)))

}
