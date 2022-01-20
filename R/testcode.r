
function() {

library(ccsAnnotator)

## create codebook
sentiment = annotation_variable('sentiment', 'assign sentiment to words',
                                codes = c(Negative = 'red', Neutral = 'grey', Positive = 'green'))

codingjob = create_job('Sotu sentiment',
                       create_units(mini_sotu, id='id', text='text', meta=c('name','year')),
                       create_codebook(sentiment))

job_db = create_job_db(codingjob, overwrite = T)


## run in current session. Auto opens annotator in browser
job_db = start_annotator(job_db)

## run in rstudio background job. Auto opens annotator in viewer (if available)
job_db = start_annotator(job_db, background = T)

gimme_annotations()   ## get annotations from current/most recent job
gimme_annotations(job_db) ## more transparent, and works across sessions

## if not auto start from start_annotator
view_annotator()  ## checks if viewer is available, otherwise use browser
view_annotator(in_browser=T)


annotations = gimme_annotations()

#annotations = gimme_annotations()
codingjob = create_job('Sotu sentiment',
                       create_units(mini_sotu, id='id', text='text', meta=c('name','year')),
                       create_codebook(sentiment),
                       annotations)

codingjob$units[[2]]$unit$importedAnnotations

job_db = create_job_db(codingjob, overwrite = T)


## run in current session. Auto opens annotator in browser
job_db = start_annotator(job_db)


## test with importing annotations
library(corpustools)

tc = create_tcorpus(mini_sotu, c('name','text'), 'id')

tc$code_dictionary(quanteda::data_dictionary_LSD2015)








}


