
create_job <- function(title, units, codebook) {
  cj_package = list(title=title, provenance=list(), codebook=codebook, units=units, rules= {ruleset: 'crowdcoding'})

  request('codingjob', json_data = jsonlite::toJSON(cj_package))
}

function() {
  codebook = create_codebook('annotate',
                  codebook_variable("sentiment", "Assign sentiment to words", codes(
                   code('negative', color='red'),
                   code('neutral', color='grey'),
                   code('positive', color='green')
                  )))

    codebook$variables[[1]]

  d = data.frame(id=c(1,2),
                title = c('title 1', 'title 2'),
                text=c('text 1','text 2'),
                pre=c('<','<'), post=c('>','>'),
                date=c('2010','2020'),
                topic=c('a','b'))

  units =  create_units(d, id='id', text=c('title','text'), meta='date', variables='topic')
  title='test'

  cj_package = list(title=jsonlite::unbox(title),
                    provenance=list(),
                    codebook=codebook,
                    units=units,
                    rules= list(ruleset= jsonlite::unbox('crowdcoding')))

  backend_connect('http://127.0.0.1:5000', 'test@user.com')
  request('codingjob', post = T, json_data = jsonlite::toJSON(cj_package))
}
