
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


####################
####################


## test with importing annotations
detach("package:corpustools", unload = TRUE)
library(corpustools)
library(ccsAnnotator)

tc = create_tcorpus(mini_sotu, c('name','text'), 'id', remember_spaces = T)
tc$code_dictionary(quanteda::data_dictionary_LSD2015, column = 'sentiment')

units = untokenize(tc)
annotations = export_span_annotations(tc, 'sentiment')
annotations$id = annotations$doc_id

unique(annotations[,c('variable','value')])
sentiment = annotation_variable('sentiment', 'Annotate the sentiment of words', only_edit = T, multiple=T,
          codes = c(positive='green', neg_negative='green', negative='red', neg_positive='red'))


codingjob = create_job('Sotu sentiment',
                       create_units(units, id='doc_id', text='text'),
                       create_codebook(sentiment),
                       annotations)




codingjob$units[[1]]$unit$text_fields
head(codingjob$units[[1]]$unit$importedAnnotations)

job_db = create_job_db(codingjob, overwrite=T)
start_annotator(job_db, background=T)



codingjob = create_job('test nieuw 1',
                       create_units(units, id='doc_id', text='text'),
                       create_codebook(sentiment),
                       annotations)

codingjob$rules

backend_connect('https://amcat4.labs.vu.nl/api/annotator', 'test@user.com')
upload_job(codingjob)
}


