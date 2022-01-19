
function() {

library(ccsAnnotator)

## create codebook
sentiment = annotation_variable('sentiment', 'assign sentiment to words',
                                codes = c(Negative = 'red', Neutral = 'grey', Positive = 'green'))
codebook = create_codebook(sentiment)

codingjob = create_job('Sotu sentiment',
                       create_units(mini_sotu, id='id', text='text', meta=c('name','year')),
                       create_codebook(sentiment))

start_annotator(codingjob, overwrite=T)


## create pipe style?
## doesn't really makes sense because of 'mandatory' parts.
## "What's with this pipe obsession?" (Mario, 1990)
# mini_sotu |>
#   create_job('sotu sentiment', id='id') |>
#   text_field('text') \>
#   create_units('id', 'text') |>
#   create_variable("sentiment", "assign sentiment to words", sentiment_codes) |>
#   start_annotator
#

## todo
## add get_annotations from either the db and server
## add simpler functions for simpler tasks. Like, annotinder does not need
## detailed text_fields, and only three answers.
}
