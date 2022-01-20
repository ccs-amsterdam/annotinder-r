
function() {

library(ccsAnnotator)

## create codebook
sentiment = annotation_variable('sentiment', 'assign sentiment to words',
                                codes = c(Negative = 'red', Neutral = 'grey', Positive = 'green'))
codebook = create_codebook(sentiment)

codingjob = create_job('Sotu sentiment',
                       create_units(mini_sotu, id='id', text='text', meta=c('name','year')),
                       create_codebook(sentiment))



## run in current session. Auto opens annotator in browser
job_db = start_annotator(codingjob, overwrite=T)

## run in rstudio background job. Auto opens annotator in viewer (if available)
job_db = start_annotator(codingjob, overwrite=T, background = T)

gimme_annotations()   ## get annotations from current/most recent job
gimme_annotations(job_db) ## more transparent, and works across sessions

## if not auto start from start_annotator
view_annotator()  ## checks if viewer is available, otherwise use browser
view_annotator(in_browser=T)

}
