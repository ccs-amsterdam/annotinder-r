function() {
  library(annotinder)


  data = data.frame(id = c(1,2,3),
                    instruction = '# Topic intrusion\n\nBelow you see five words. Four of these words are related to the same topic, but one word is an "imposter". Please select the imposter word',
                    w1 = c('some','example','words'),
                    w2 = c('more','beautiful','terms'),
                    w3 = c('I','am','getting'),
                    w4 = c('really','tired','off'),
                    w5 = c('coming','up','with'))
  data

  units = create_units(data, id='id',
                       text = set_markdown(instruction),
                       set_question('question', codes=c(w1,w2,w3,w4,w5)))


  create_job('test', units) %>%
    create_job_db(overwrite=T) %>%
    start_annotator(background=T)


  data = data.frame(id = c(1,2,3,4,5),
                    type = c('train','code','test','code','test'),
                    letter = letters[1:5],
                    date=c('2020-01-01','2020-01-02','2020-01-03','2020-01-04','2020-01-05'),
                    source=c('imagination'),
                    title=c('Cat','Cat','Dog','Dog','Car'),
                    text= c('I like cats.',
                            "Cats are awesome.",
                            "Some people like dogs.",
                            "Dogs are pretty awesome too.",
                            "Other people like cars"),
                    image=c('https://cdn.pixabay.com/photo/2017/07/25/01/22/cat-2536662_960_720.jpg',
                            'https://cdn.pixabay.com/photo/2014/04/13/20/49/cat-323262_960_720.jpg',
                            'https://cdn.pixabay.com/photo/2018/01/09/11/04/dog-3071334_960_720.jpg',
                            'https://cdn.pixabay.com/photo/2017/09/25/13/14/dog-2785077_960_720.jpg',
                            'https://cdn.pixabay.com/photo/2016/11/29/09/32/auto-1868726_960_720.jpg'),
                    caption=c('Cat!','Caaaaaat','Doggie!!','Dog','Crrr'),
                    markdown=c('**useless markdown text**'),
                    animal=c('Cat',NA,'Dog',NA, 'Neither :('),
                    animal_hint=c("Hint: look closely at those ears and paws.", NA, NA, NA,NA))


  units = create_units(data, id='id', type='type', meta=c('date','source'),
    set_text('title', title, text_size=2, bold=T, align='center'),
    set_text('text', text, align='center'),
    set_image('image', image, caption=caption),
    set_markdown('markdown', markdown, align='center'),
    set_train('animal', animal,
              message='# OH NOES!!\n\nThis was a training unit, and it seems you got it wrong!',
              submessage=animal_hint),
    set_test('animal', animal, damage=10)
  )

  ## add a set_type or something. Because its not cool that set_train would now need to select for every variable

  codebook = create_codebook(
    sentiment = question('animal', 'What animal is this?', type = 'annotinder',
                         codes = c('Cat','Dog','Neither :('))
  )

  create_job('test', units, codebook) %>%
    create_job_db(overwrite=T) %>%
    start_annotator(background=T)

  js = list(
    jobset())

  upload_job('5', units=units, codebook=codebook, rules=rules_fixedset(randomize=T))


  u = prepare_units(units)
  jsonlite::toJSON(u[[1]], pretty = T, auto_unbox = T)
}
