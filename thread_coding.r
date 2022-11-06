library(annotinder)
library(tidyverse)

tweets = data.frame(conversation = c(1,1,1,1,2,2),
               tweet = c('heeeeejj', 'Owh hey!', 'Daar ben ik het niet mee eens!', 'Goed punt', 'Ik ben jarig','Gefeliciteerd!'),
               author = c('Author','Comment','Author','Comment','Author','Comment'),
               type = c('tweet','comment','comment','comment','tweet','comment'))

first_tweet = tweets %>% filter(type == 'tweet') %>% select(conversation, tweet)
comments = tweets %>% filter(type == 'comment') %>% select(conversation, comment = tweet, author)
d = first_tweet %>% full_join(comments, by='conversation')

testdit = text_field(tweet, margin = '10px 25% 10px 10px')

units = create_units(d, id='conversation', subfields='comment',
  tweet = testdit,
  comment = text_field(comment, align= ifelse(author == 'Author', 'left','right'), margin = ifelse(author == 'Author', '10px 25%px 10px 10px', '10px 10px 10px 25%'))
)

codebook = create_codebook(
  test = question('klimaatverandering', 'Is het eerste bericht in deze conversatie blablabla?', type='buttons',
            field='tweet',
            codes = c('Nee','Ja')),
  dit = question('reactie', 'Wat is de toon van deze reactie?', type='buttons',
            per_field = 'comment',
            codes = c('Boos','Blij','Irritant'))
)

create_job('Thread coding', units, codebook) %>%
  create_job_db(overwrite=T) %>%
  start_annotator(background = T)



x = prepare_units(units)

jsonlite::toJSON(x[[1]], pretty = T)
names(x[[1]]$unit$text_fields[[2]]$value) = NULL
