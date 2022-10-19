library(annotinder)
library(tidyverse)

tweets = data.frame(conversation = c(1,1,1,1,2,2),
               tweet = c('heeeeejj', 'Owh hey!', 'Daar ben ik het niet mee eens!', 'Goed punt', 'Ik ben jarig','Gefeliciteerd!'),
               type = c('tweet','comment','comment','comment','tweet','comment'))

conversations = tweets %>%
  group_by(conversation, type) %>%
  summarize(tweet = list(tweet)) %>%
  pivot_wider(conversation, names_from=type, values_from=tweet)

units = create_units(conversations, id='conversation') %>%
  set_text('tweet', label='TWEET', fontWeight='bold', borderBottom= '1px solid black', marginBottom= '5px') %>%
  set_text('comment', label='REACTIE') %>%
  set_grid(list(c('tweet', 'tweet'),
                c('.'    , 'comment')), columns = c(1,6))

codebook = create_codebook(
  test = question('klimaatverandering', 'Gaat deze tweet over klimaatverandering?', type='buttons',
            fields='tweet',
            codes = c('Nee','Ja')),
  dit = question('reactie', 'Wat is de toon van deze reactie?', type='buttons',
            per_field = 'comment',
            codes = c('Boos','Blij','Irritant'))
)

create_job('Thread coding', units, codebook) %>%
  create_job_db(overwrite=T) %>%
  start_annotator(background = T)



