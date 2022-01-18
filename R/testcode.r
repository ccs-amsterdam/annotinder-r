
function() {

library(ccsAnnotator)
sentiment_codes = c(Negative = 'red', Neutral = 'grey', Positive = 'green')

codebook = create_codebook('annotate') |>
  add_variable("sentiment", "assign sentiment to words", sentiment_codes)

units = create_units(mini_sotu,  text='text', meta=c('name','year'))

codingjob = create_job('Sotu sentiment', units, codebook)

start_annotator(codingjob, overwrite=T)


## todo
## add get_annotations from either the db and server
## add simpler functions for simpler tasks. Like, annotinder does not need
## detailed text_fields, and only three answers.
}
