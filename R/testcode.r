
function() {

library(ccsAnnotator)

## create codebook
sentiment = annotation_variable('sentiment', 'assign sentiment to words',
                                codes = c(Negative = 'red', Neutral = 'grey', Positive = 'green'))
codebook = create_codebook(sentiment)

codingjob = create_job('Sotu sentiment',
                       create_units(mini_sotu, id='id', text='text', meta=c('name','year')),
                       create_codebook(sentiment))

start_annotator(codingjob, overwrite=T, background=T)


show_in_viewer()

}
