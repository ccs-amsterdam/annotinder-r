
function() {

library(annotinder)
backend_connect('http://localhost:5000', 'kasperwelbers@gmail.com')

## create codebook
sentiment = question('sentiment', 'assign sentiment to words',
                     codes = c(Negative = 'crimson', Neutral = 'grey', Positive = 'lightgreen'))
codebook = create_codebook(sentiment)


units = create_units(mini_sotu_par, 'id') %>%
  set_meta(c('name','year','paragraph')) %>%
  set_text('text')


job = create_job('test', units, codebook)
job_db = create_job_db(job, overwrite = T)
start_annotator(job_db, background = T)

job_db

units = create_units(mini_sotu_par, 'id') %>%
  set_text('text')
job = create_job('test2', units, codebook)
job_db = create_job_db(job, overwrite = T)
start_annotator(job_db, background = T)


file.exists('/home/kasper/projects/annotinder-r/annotinder_jobs/tests.db')

start_annotator('/home/kasper/projects/annotinder-r/annotinder_jobs/test.db', background = T)


upload_job('a', units=units, codebook=codebook)


codebook_swipe = create_codebook(
  sentiment = question('sentiment', 'assign sentiment to words', type = 'annotinder',
                       codes = c(Negative = 'crimson', Positive = 'green', Neutral = 'grey'))
)

jobsets = list(
  jobset('2 items', ids=head(mini_sotu_par$id, 2)),
  jobset('2 items, swiping', ids=head(mini_sotu_par$id, 3), codebook=codebook_swipe),
  jobset('5 items rev', ids=rev(head(mini_sotu_par$id, 5)))
)


upload_job('single fixed set', units=units, codebook=codebook)
upload_job('3 fixed sets', units=units, codebook=codebook, jobsets=jobsets)
upload_job('single crowd set', units=units, codebook=codebook, rules=rules_crowdcoding())
upload_job('3 crowd sets', units=units, codebook=codebook, jobsets=jobsets, rules=rules_crowdcoding())


info$unit$text_fields

demo$unit$text_fields
demo = create_question_unit('Welkom!', "Hieronder vragen wij u eerst enkele vragen over wie u bent",
                     question('geslacht', 'Wat is uw geslacht?',
                              codes = c(Man='grey', Vrouw='grey', anders='grey', `wil ik niet zeggen`='grey')),
                     question('opleiding', 'Wat is uw hoogst afgeronde opleidingsniveau?',
                              codes = c('Middelbaar of lager', 'MBO', "HBO", "Universiteit" )))
info = create_info_unit('Geloofwaardigheids-Tinder', "Zodadelijk ziet u een reeks nieuwsberichten voorbijkomen. Als u het bericht geloofwaardig vindt, swipe dan naar rechts (of klik op de knoppen onderaan het scherm). Als u twijfelt aan de geloofwaardigheid van het bericht, swipe dan naar Links")
pre = list(demo, info)

upload_job('with intro', units=units, codebook=codebook, pre=list(demo, info))
upload_job('with intro sets', units=units, codebook=codebook, pre=list(demo, info), jobsets = jobsets)






library(corpustools)
library(annotinder)

## create codebook
sentiment = question('sentiment', 'assign sentiment to words',
                                codes = c(Negative = 'red', Neutral = 'grey', Positive = 'green'))

codingjob = create_job('Sotu sentiment crowd',
                        create_units(mini_sotu, id='id', text='text', meta=c('name','year')),
                        create_codebook(sentiment))

job_db = create_job_db(codingjob, overwrite=T)
start_annotator(job_db, background=T)


mini_sotu$id = c('test','of','ID','werkt')


codingjob1 = create_job('Sotu sentiment crowd',
                       create_units(mini_sotu, id='id', text='text', meta=c('name','year')),
                       create_codebook(sentiment))

codingjob2 = create_job('Sotu sentiment crowd n=2 back=F',
                       create_units(mini_sotu, id='id', text='text', meta=c('name','year')),
                       create_codebook(sentiment))

codingjob3 = create_job('Sotu sentiment fixed',
                       create_units(mini_sotu, id='id', text='text', meta=c('name','year')),
                       create_codebook(sentiment))

codingjob4 = create_job('Sotu sentiment fixed forward=T',
                       create_units(sotu_texts, id='id', text='text'),
                       create_codebook(sentiment))

codingjob5 = create_job('Sotu sentiment crowd',
                        create_units(sotu_texts, id='id', text='text'),
                        create_codebook(sentiment))

codingjob6 = create_job('Sotu sentiment fixed 5 sets',
                        create_units(sotu_texts, id='id', text='text'),
                        create_codebook(sentiment))


backend_connect('http://localhost:5000', 'test@user.com')

upload_job(codingjob1)
upload_job(codingjob2, rules_crowdcoding(units_per_coder=2, can_seek_backwards = F))
upload_job(codingjob3, rules_fixedset())
upload_job(codingjob4, rules_fixedset(can_seek_forwards = T))
upload_job(codingjob5, rules_crowdcoding())

sets = list(
  group1 = head(sotu_texts$id, 2),
  group2 = head(sotu_texts$id, 3),
  group3 = head(sotu_texts$id, 4),
  group4 = rev(head(sotu_texts$id, 5))
)


upload_job(codingjob1, rules_crowdcoding(sets=sets))
upload_job(codingjob6, rules_fixedset(sets=sets))

upload_job(list())

job_db = create_job_db(codingjob5, overwrite=T)
start_annotator(job_db, background=T)


backend_connect('https://amcat4.labs.vu.nl/api/annotator', 'test@user.com')
upload_job(codingjob5)


test = codingjob$units[[1]]
test$id


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
library(corpustools)
library(annotinder)

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



job_db = create_job_db(codingjob, overwrite=T)
start_annotator(job_db, background=T)




codingjob = create_job('test nieuw 1',
                       create_units(units, id='doc_id', text='text'),
                       create_codebook(sentiment),
                       annotations)

codingjob$rules

backend_connect('http://localhost:5000/annotator', 'test@user.com')
upload_job(codingjob)
}


